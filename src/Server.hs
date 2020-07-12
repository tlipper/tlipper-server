{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Aeson.Extra (jsonPrefix)
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (find)
import Data.Foldable (for_)
import Data.IORef
import qualified Data.Text as T
import qualified Database as DB
import qualified Database as DB
import qualified Database.Esqueleto as ESQ
import Database.Middleware
import qualified Database.Persist as DB
import GHC.Generics
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client (ClientEnv, runClientM)
import Servant.Server
import qualified Twitch.API as Twitch
import qualified Twitch.Vod as Twitch

type VideoId = T.Text

type UserId = Int

data SyncVideosRequest =
  SyncVideosRequest
    { _svrUserId :: Int
    , _svrUserName :: T.Text
    }
  deriving (Generic, Show, Eq)

type ListChannels = "channels" :> Get '[ JSON] [Twitch.Channel]

type AddChannel
   = "channels" :> QueryParam' '[ Required] "name" T.Text :> Post '[ JSON] Twitch.Channel

type ListVideos
   = "videos" :> QueryParam' '[ Required] "channel_id" T.Text :> Get '[ JSON] [Twitch.Video]

type ListClips
   = "videos" :> Capture' '[ Required] "video_id" VideoId :> "clips" :> Get '[ JSON] [Twitch.Clip]

type DownloadVideo
   = "download" :> ReqBody '[ JSON] VideoId :> Post '[ JSON] Twitch.Video

type SyncVideo
   = "videos" :> "sync" :> ReqBody '[ JSON] SyncVideosRequest :> Post '[ JSON] ()

type API
   = ListChannels :<|> AddChannel :<|> ListVideos :<|> DownloadVideo :<|> SyncVideo :<|> ListClips

data ServerState =
  ServerState
    { _ssTwitchClientEnv :: ClientEnv
    , _ssAwsCredentials :: AWS.Credentials
    }

serveFromDatabase ::
     ( DB.BackendCompatible ESQ.SqlBackend (DB.PersistEntityBackend a)
     , DB.PersistEntity a
     , FromDatabase a b
     )
  => IORef ServerState
  -> DB.SqlCtrl
  -> Handler [b]
serveFromDatabase state_ref (DB.SqlCtrl runSql) = do
  state <- liftIO $ readIORef state_ref
  dbChannels <-
    liftIO $
    fmap (map DB.entityVal) $ runSql (ESQ.select $ ESQ.from $ \v -> return v)
  pure $ map fromDatabase dbChannels

server :: IORef ServerState -> DB.SqlCtrl -> Server API
server state_ref sqlCtrl@(DB.SqlCtrl runSql) =
  listChannels :<|> addChannel :<|> listVideos :<|> downloadVideo :<|>
  syncVideos :<|>
  listClips
  where
    listChannels :: Handler [Twitch.Channel]
    listChannels = do
      serveFromDatabase state_ref sqlCtrl
    addChannel :: T.Text -> Handler Twitch.Channel
    addChannel channel_name = do
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      liftIO
        (runClientM (Twitch.searchPaginatedChannels channel_name) clientEnv) >>= \case
        Left clientErr ->
          throwError err500 {errBody = LBS8.pack (show clientErr)}
        Right channels -> do
          case find ((== channel_name) . Twitch._chDisplayName) channels of
            Nothing ->
              throwError err404 {errBody = "Could not find the channel."}
            Just c -> do
              liftIO $ runSql $ ESQ.insert (toDatabase c)
              pure c
    listVideos :: T.Text -> Handler [Twitch.Video]
    listVideos channel_id = do
      state <- liftIO $ readIORef state_ref
      dbChannels <-
        liftIO $
        fmap (map DB.entityVal) $
        runSql
          (ESQ.select $
           ESQ.from $ \v -> do
             ESQ.where_ ((v ESQ.^. DB.VUserId) ESQ.==. (ESQ.val channel_id))
             return v)
      pure $ map fromDatabase dbChannels
    downloadVideo :: VideoId -> Handler Twitch.Video
    downloadVideo video_id = do
      state <- liftIO $ readIORef state_ref
      videos <-
        liftIO $
        fmap (map DB.entityVal) $
        runSql (ESQ.select $ ESQ.from $ \v -> return v)
      case find ((== video_id) . DB.vVid) videos of
        Just (fromDatabase -> video) -> do
          ei <-
            runExceptT $
            Twitch.downloadVideo
              (_ssAwsCredentials state)
              (_ssTwitchClientEnv state)
              video
          either (\e -> throwError err400 {errBody = LBS8.pack e}) pure ei
          pure video
        Nothing -> throwError err400
    syncVideos :: SyncVideosRequest -> Handler ()
    syncVideos (SyncVideosRequest user_id user_name) = do
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      liftIO (runClientM (Twitch.listPaginatedVideos (Just user_id)) clientEnv) >>= \case
        Left clientErr ->
          throwError err500 {errBody = LBS8.pack (show clientErr)}
        Right videos -> do
          liftIO $ runSql $ DB.putMany $ map toDatabase videos
          let videoIds = map Twitch._vId videos
          liftIO
            (runClientM
               (Twitch.listPaginatedClips
                  (Just (T.unpack user_name))
                  ((`elem` videoIds) . Twitch._cvId . Twitch._cVod))
               clientEnv) >>= \case
            Left clientErr ->
              throwError err500 {errBody = LBS8.pack (show clientErr)}
            Right clips -> liftIO $ runSql $ DB.putMany $ map toDatabase clips
    listClips :: VideoId -> Handler [Twitch.Clip]
    listClips video_id =
      liftIO $
      fmap (map (fromDatabase . DB.entityVal)) $
      runSql
        (ESQ.select $
         ESQ.from $ \v -> do
           ESQ.where_ ((v ESQ.^. DB.CVodId) ESQ.==. (ESQ.val video_id))
           return v)

myApi :: Proxy API
myApi = Proxy

app :: IORef ServerState -> DB.SqlCtrl -> Application
app state sqlCtrl = allowCors $ serve myApi (server state sqlCtrl)

runServer :: ClientEnv -> AWS.Credentials -> Int -> DB.SqlCtrl -> IO ()
runServer twitchClientEnv awsCredentials port sqlCtrl = do
  state <- newIORef (ServerState twitchClientEnv awsCredentials)
  putStrLn $ "Server is running on port " <> show port <> "..."
  Warp.run port (app state sqlCtrl)

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

$(JSON.deriveJSON jsonPrefix ''SyncVideosRequest)
