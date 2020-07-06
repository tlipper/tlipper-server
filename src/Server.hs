{-# LANGUAGE DataKinds #-}
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
import qualified Database.Esqueleto as DB
import Database.Middleware
import qualified Database.Persist as DB
import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
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

type API
   = "videos" :> Get '[ JSON] [Twitch.Video] :<|> "download" :> ReqBody '[ JSON] VideoId :> Post '[ JSON] Twitch.Video :<|> "videos" :> "sync" :> ReqBody '[ JSON] SyncVideosRequest :> Post '[ JSON] ()

data ServerState =
  ServerState
    { _ssTwitchClientEnv :: ClientEnv
    , _ssAwsCredentials :: AWS.Credentials
    }

server :: IORef ServerState -> DB.SqlCtrl -> Server API
server state_ref (DB.SqlCtrl runSql) =
  listVideos :<|> downloadVideo :<|> syncVideos
  where
    listVideos :: Handler [Twitch.Video]
    listVideos = do
      state <- liftIO $ readIORef state_ref
      dbVideos <-
        liftIO $
        fmap (map DB.entityVal) $ runSql (DB.select $ DB.from $ \v -> return v)
      pure $ map fromDatabase dbVideos
    downloadVideo :: VideoId -> Handler Twitch.Video
    downloadVideo video_id = do
      state <- liftIO $ readIORef state_ref
      videos <-
        liftIO $
        fmap (map DB.entityVal) $ runSql (DB.select $ DB.from $ \v -> return v)
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

myApi :: Proxy API
myApi = Proxy

app :: IORef ServerState -> DB.SqlCtrl -> Application
app state sqlCtrl = serve myApi (server state sqlCtrl)

runServer :: ClientEnv -> AWS.Credentials -> Int -> DB.SqlCtrl -> IO ()
runServer twitchClientEnv awsCredentials port sqlCtrl = do
  state <- newIORef (ServerState twitchClientEnv awsCredentials)
  putStrLn $ "Server is running on port " <> show port <> "..."
  Warp.run port (app state sqlCtrl)

$(JSON.deriveJSON jsonPrefix ''SyncVideosRequest)
