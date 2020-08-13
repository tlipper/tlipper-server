{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Aeson.Extra (jsonPrefix)
import Conduit
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted (async)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (find)
import Data.Foldable (for_)
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database as DB
import qualified Database as DB
import qualified Database.Esqueleto as ESQ
import Database.Middleware
import qualified Database.Persist as DB
import Database.Persist
import GHC.Generics
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client (ClientEnv, ClientM, runClientM)
import Servant.Server
import qualified Twitch.API as Twitch
import qualified Twitch.Analytics as Twitch
import qualified Twitch.Vod as Twitch
import System.FilePath ((</>))

data ExportSegment =
  ExportSegment
    { _esStartTimestamp :: Int
    , _esEndTimestamp :: Int
    }
  deriving (Show)

data TakeExportRequest =
  TakeExportRequest
    { _terVideoId :: VideoId
    , _terExportSegments :: [ExportSegment]
    }
  deriving (Show)

data TakeExportResponse =
  TakeExportResponse
    { _terExportId :: Key DB.Export -- It's a very bad idea to couple DB type in response type. Refactor this.
    }
  deriving (Show)

type VideoId = T.Text

type UserId = Int

type ListChannels = "channels" :> Get '[ JSON] [Twitch.Channel]

type AddChannel
   = "channels" :> QueryParam' '[ Required] "name" T.Text :> Post '[ JSON] Twitch.Channel

type ListVideos
   = "videos" :> QueryParam' '[ Required] "channel_id" T.Text :> Get '[ JSON] [Twitch.Video]

type ListClips
   = "videos" :> Capture' '[ Required] "video_id" VideoId :> "clips" :> Get '[ JSON] [Twitch.Clip]

type SyncClips
   = "videos" :> Capture' '[ Required] "video_id" VideoId :> "clips" :> "sync" :> Post '[ JSON] ()

type GetVideoAnalysis
   = "videos" :> Capture' '[ Required] "video_id" VideoId :> "analysis" :> Get '[ JSON] Twitch.VideoAnalytics

-- type DownloadVideo
--    = "download" :> ReqBody '[ JSON] VideoId :> Post '[ JSON] T.Text
type SyncVideo
   = "videos" :> "sync" :> QueryParam' '[ Required] "channel_id" T.Text :> Post '[ JSON] ()

type TakeExport
   = "exports" :> ReqBody '[ JSON] TakeExportRequest :> Post '[ JSON] TakeExportResponse

type API
   = ListChannels :<|> AddChannel :<|> ListVideos :<|> SyncVideo :<|> ListClips :<|> SyncClips :<|> GetVideoAnalysis :<|> TakeExport

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

sqlFindOr404 ::
     DB.SqlCtrl
  -> String
  -> ReaderT ESQ.SqlBackend (NoLoggingT (ResourceT IO)) (Maybe a)
  -> Handler a
sqlFindOr404 (DB.SqlCtrl runSql) resourceName uniqueRecord =
  liftIO (runSql uniqueRecord) >>= \case
    Just record -> pure record
    Nothing ->
      throwError
        err404
          { errBody =
              "Could not find the " <> LBS8.pack resourceName <> " resource."
          }

runLiftedClientM :: ClientM a -> ClientEnv -> Handler a
runLiftedClientM act env =
  liftIO (runClientM act env) >>= \case
    Left clientErr -> throwError err500 {errBody = LBS8.pack (show clientErr)}
    Right result -> pure result

server :: IORef ServerState -> DB.SqlCtrl -> Server API
server state_ref sqlCtrl@(DB.SqlCtrl runSql) =
  listChannels :<|> addChannel :<|> listVideos :<|> syncVideos :<|> listClips :<|>
  syncClips :<|>
  getVideoAnalysis :<|>
  takeExportRequest
  where
    listChannels :: Handler [Twitch.Channel]
    listChannels = do
      serveFromDatabase state_ref sqlCtrl
    addChannel :: T.Text -> Handler Twitch.Channel
    addChannel channel_name = do
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      channels <-
        runLiftedClientM (Twitch.paginateBlocking channel_name 100) clientEnv
      case find ((== channel_name) . Twitch._chDisplayName) channels of
        Nothing -> throwError err404 {errBody = "Could not find the channel."}
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
    -- downloadVideo :: VideoId -> Handler T.Text
    -- downloadVideo video_id = do
    --   state <- liftIO $ readIORef state_ref
    --   video <-
    --     sqlFindOr404 sqlCtrl "Video" (ESQ.getBy (DB.UniqueTwitchVideoId video_id))
    --   liftIO
    --     (runSql (ESQ.getBy (DB.UniqueVideoUrlVideoId (DB.entityKey video)))) >>= \case
    --     Just (DB.entityVal -> video_url) -> pure $ DB.vuUrl video_url
    --     Nothing -> do
    --       runExceptT
    --         (Twitch.downloadVideo
    --            (_ssTwitchClientEnv state)
    --            (fromDatabase (DB.entityVal video))) >>= \case
    --         Left err -> throwError err400 {errBody = LBS8.pack err}
    --         Right url -> do
    --           liftIO $
    --             runSql $ ESQ.insert (DB.VideoUrl (DB.entityKey video) url)
    --           pure url
    syncVideos :: T.Text -> Handler ()
    syncVideos channel_id = do
      state <- liftIO $ readIORef state_ref
      (DB.entityVal -> channel) <-
        sqlFindOr404 sqlCtrl "Channel" $
        ESQ.getBy (DB.UniqueChannelId channel_id)
      let clientEnv = _ssTwitchClientEnv state
      (Set.toList -> videos) <-
        flip runLiftedClientM clientEnv $
        Twitch.paginateBlocking (Just (DB.chChId channel)) 1000
      liftIO $ runSql $ DB.putMany $ map toDatabase videos
      let videoIds = map Twitch._vId videos
      flip runLiftedClientM clientEnv $
        Twitch.paginate (Just (T.unpack (DB.chDisplayName channel))) 1000 $ \_ (Set.toList -> clips) -> do
          let filtered_clips =
                filter ((`elem` videoIds) . Twitch._cvId . Twitch._cVod) clips
          liftIO $ runSql $ DB.putMany $ map toDatabase filtered_clips
    listClips :: VideoId -> Handler [Twitch.Clip]
    listClips video_id =
      liftIO $
      fmap (map (fromDatabase . DB.entityVal)) $
      runSql
        (ESQ.select $
         ESQ.from $ \v -> do
           ESQ.where_ ((v ESQ.^. DB.CVodId) ESQ.==. (ESQ.val video_id))
           return v)
    syncClips :: VideoId -> Handler ()
    syncClips video_id = do
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      (fromDatabase . DB.entityVal -> video) <-
        sqlFindOr404 sqlCtrl "Video" (ESQ.getBy (DB.UniqueTwitchVideoId video_id))
      flip runLiftedClientM clientEnv $
        Twitch.paginate (Just (T.unpack (Twitch._vUserName video))) 1000 $ \_ (Set.toList -> clips) -> do
          let filtered_clips =
                filter ((== video_id) . Twitch._cvId . Twitch._cVod) clips
          liftIO $ runSql $ DB.putMany $ map toDatabase filtered_clips
    getVideoAnalysis :: VideoId -> Handler Twitch.VideoAnalytics
    getVideoAnalysis video_id = do
      (fromDatabase . DB.entityVal -> video) <-
        sqlFindOr404 sqlCtrl "Video" (ESQ.getBy (DB.UniqueTwitchVideoId video_id))
      clips <- listClips video_id
      runExceptT (Twitch.analyse video clips) >>= \case
        Left err -> throwError err500 {errBody = LBS8.pack err}
        Right v -> pure v
    takeExportRequest :: TakeExportRequest -> Handler TakeExportResponse
    takeExportRequest (TakeExportRequest twitchVideoId segments) = do
      video <-
        sqlFindOr404 sqlCtrl "Video" (ESQ.getBy (DB.UniqueTwitchVideoId twitchVideoId))
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      exportId <- liftIO $ runSql $ ESQ.insert $ DB.Export Nothing
      async $ takeExport clientEnv sqlCtrl video exportId segments
      pure $ TakeExportResponse exportId

takeExport ::
     ClientEnv
  -> DB.SqlCtrl
  -> DB.Entity DB.Video
  -> DB.Key DB.Export
  -> [ExportSegment]
  -> Handler ()
takeExport _ _ _ _ [] = do
  liftIO $ putStrLn "Not taking export"
  pure ()
takeExport clientEnv sqlCtrl@(DB.SqlCtrl runSql) video exportId exportSegments = do
  let serializedSegments =
        map (_esStartTimestamp &&& _esEndTimestamp) exportSegments
  liftIO $ putStrLn "Taking export..."
  runExceptT
    (Twitch.downloadVideo
       clientEnv
       (fromDatabase (DB.entityVal video))
       serializedSegments
       ("export" <> show (ESQ.fromSqlKey exportId))
       ("exports" </> show (ESQ.fromSqlKey exportId))) >>= \case
    Left downloadError -> liftIO $ print downloadError
    Right mp4_url ->
      liftIO $ runSql $ ESQ.update $ \e -> do
        ESQ.set e [ DB.EUrl ESQ.=. ESQ.just (ESQ.val (T.pack mp4_url)) ]
        ESQ.where_ $ e ESQ.^. DB.ExportId ESQ.==. (ESQ.val exportId)

myApi :: Proxy API
myApi = Proxy

app :: IORef ServerState -> DB.SqlCtrl -> Application
app state sqlCtrl = allowCors $ serve myApi (server state sqlCtrl)

runServer :: ClientEnv -> AWS.Credentials -> Int -> DB.SqlCtrl -> IO ()
runServer twitchClientEnv awsCredentials port sqlCtrl = do
  state <- newIORef (ServerState twitchClientEnv awsCredentials)
  putStrLn $ "Server is running on port " <> show port <> "..."
  withStdoutLogger $ \logger ->
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
     in Warp.runSettings settings (app state sqlCtrl)

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

$(JSON.deriveJSON jsonPrefix ''TakeExportRequest)

$(JSON.deriveJSON jsonPrefix ''TakeExportResponse)

$(JSON.deriveJSON jsonPrefix ''ExportSegment)
