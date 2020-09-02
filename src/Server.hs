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

import qualified AWS.API as AWS
import Aeson.Extra (jsonPrefix)
import Conduit
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.Chan.Class
import Control.Concurrent.Timeout
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
import Data.Int
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database as DB
import qualified Database.Esqueleto as ESQ
import Database.Middleware
import qualified Database.Persist as DB
import Database.Persist
import GHC.Generics
import Monitoring (AppMetrics(..), ServantMetrics)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client (ClientEnv, ClientM, runClientM)
import Servant.Server
import Server.API
import Server.API.Monitoring (monitorServant)
import System.FilePath ((</>))
import System.Log.FastLogger
import qualified Twitch.API as Twitch
import qualified Twitch.Analytics as Twitch
import qualified Twitch.Vod as Twitch

data ServerState =
  ServerState
    { _ssTwitchClientEnv :: ClientEnv
    , _ssAwsCredentials :: AWS.Credentials
    }

logM :: ToLogStr msg => msg -> AppM ()
logM msg = do
  loggerSet <- asks _acLoggerSet
  liftIO $ pushLogStrLn loggerSet $ toLogStr msg

runSqlM ::
     forall a. ReaderT ESQ.SqlBackend (NoLoggingT (ResourceT IO)) a -> AppM a
runSqlM q = do
  DB.SqlCtrl runSql <- asks _acSqlCtrl
  liftIO $ runSql q

serveFromDatabase ::
     ( DB.BackendCompatible ESQ.SqlBackend (DB.PersistEntityBackend a)
     , DB.PersistEntity a
     , FromDatabase a b
     )
  => AppM [b]
serveFromDatabase = do
  dbChannels <-
    fmap (map DB.entityVal) $ runSqlM (ESQ.select $ ESQ.from $ \v -> return v)
  pure $ map fromDatabase dbChannels

sqlFindOr404 ::
     String
  -> ReaderT ESQ.SqlBackend (NoLoggingT (ResourceT IO)) (Maybe a)
  -> AppM a
sqlFindOr404 resourceName uniqueRecord = do
  runSqlM uniqueRecord >>= \case
    Just record -> pure record
    Nothing ->
      throwError
        err404
          { errBody =
              "Could not find the " <> LBS8.pack resourceName <> " resource."
          }

data AppCtx =
  AppCtx
    { _acTwitchClientEnv :: ClientEnv
    , _acAwsCredentials :: AWS.Credentials
    , _acSqlCtrl :: DB.SqlCtrl
    , _acLoggerSet :: LoggerSet
    }

type AppM = ReaderT AppCtx Handler

runLiftedClientM :: ClientM a -> ClientEnv -> AppM a
runLiftedClientM act env =
  liftIO (runClientM act env) >>= \case
    Left clientErr -> throwError err500 {errBody = LBS8.pack (show clientErr)}
    Right result -> pure result

server :: ServerT API AppM
server =
  listChannels :<|> addChannel :<|> listVideos :<|> syncVideos :<|> listClips :<|>
  syncClips :<|>
  getVideoAnalysis :<|>
  listExports :<|>
  getExport :<|>
  takeExportRequest
  where
    listChannels :: AppM [Twitch.Channel]
    listChannels = do
      serveFromDatabase
    addChannel :: T.Text -> AppM Twitch.Channel
    addChannel channel_name = do
      clientEnv <- asks _acTwitchClientEnv
      channels <-
        runLiftedClientM (Twitch.paginateBlocking channel_name 100) clientEnv
      case find ((== channel_name) . Twitch._chDisplayName) channels of
        Nothing -> throwError err404 {errBody = "Could not find the channel."}
        Just c -> do
          runSqlM $ ESQ.insert (toDatabase c)
          pure c
    listVideos :: T.Text -> AppM [Twitch.Video]
    listVideos channel_id = do
      dbChannels <-
        fmap (map DB.entityVal) $
        runSqlM
          (ESQ.select $
           ESQ.from $ \v -> do
             ESQ.where_ ((v ESQ.^. DB.VUserId) ESQ.==. (ESQ.val channel_id))
             return v)
      pure $ map fromDatabase dbChannels
    syncVideos :: T.Text -> AppM ()
    syncVideos channel_id = do
      (DB.entityVal -> channel) <-
        sqlFindOr404 "Channel" $ ESQ.getBy (DB.UniqueChannelId channel_id)
      clientEnv <- asks _acTwitchClientEnv
      (Set.toList -> videos) <-
        flip runLiftedClientM clientEnv $
        Twitch.paginateBlocking (Just (DB.chChId channel)) 1000
      runSqlM $ DB.putMany $ map toDatabase videos
      let videoIds = map Twitch._vId videos
      DB.SqlCtrl runSql <- asks _acSqlCtrl
      flip runLiftedClientM clientEnv $
        Twitch.paginate (Just (T.unpack (DB.chDisplayName channel))) 1000 $ \_ (Set.toList -> clips) -> do
          let filtered_clips =
                filter ((`elem` videoIds) . Twitch._cvId . Twitch._cVod) clips
          liftIO $ runSql $ DB.putMany $ map toDatabase filtered_clips
    listClips :: VideoId -> AppM [Twitch.Clip]
    listClips video_id = do
      fmap (map (fromDatabase . DB.entityVal)) $
        runSqlM
          (ESQ.select $
           ESQ.from $ \v -> do
             ESQ.where_ ((v ESQ.^. DB.CVodId) ESQ.==. (ESQ.val video_id))
             return v)
    syncClips :: VideoId -> AppM ()
    syncClips video_id = do
      clientEnv <- asks _acTwitchClientEnv
      DB.SqlCtrl runSql <- asks _acSqlCtrl
      (fromDatabase . DB.entityVal -> video) <-
        sqlFindOr404 "Video" (ESQ.getBy (DB.UniqueTwitchVideoId video_id))
      flip runLiftedClientM clientEnv $
        Twitch.paginate (Just (T.unpack (Twitch._vUserName video))) 1000 $ \_ (Set.toList -> clips) -> do
          let filtered_clips =
                filter ((== video_id) . Twitch._cvId . Twitch._cVod) clips
          liftIO $ runSql $ DB.putMany $ map toDatabase filtered_clips
    getVideoAnalysis :: VideoId -> AppM Twitch.VideoAnalytics
    getVideoAnalysis video_id = do
      (fromDatabase . DB.entityVal -> video) <-
        sqlFindOr404 "Video" (ESQ.getBy (DB.UniqueTwitchVideoId video_id))
      clips <- listClips video_id
      runExceptT (Twitch.analyse video clips) >>= \case
        Left err -> throwError err500 {errBody = LBS8.pack err}
        Right v -> pure v
    takeExportRequest :: TakeExportRequest -> AppM TakeExportResponse
    takeExportRequest (TakeExportRequest twitchVideoId segments) = do
      awsCredentials <- asks _acAwsCredentials
      clientEnv <- asks _acTwitchClientEnv
      video <-
        sqlFindOr404 "Video" (ESQ.getBy (DB.UniqueTwitchVideoId twitchVideoId))
      takeExportAsync clientEnv awsCredentials video segments >>= \case
        Left err -> throwError err400 {errBody = "\"" <> LBS8.pack err <> "\""}
        Right exportId -> pure $ TakeExportResponse exportId
    listExports :: AppM [DB.Export]
    listExports = do
      fmap (map DB.entityVal) $ runSqlM (ESQ.select $ ESQ.from pure)
    getExport :: Int64 -> AppM DB.Export
    getExport exportId = do
      sqlFindOr404 "Export" (ESQ.get (ESQ.toSqlKey exportId))

takeExportAsync ::
     ClientEnv
  -> AWS.Credentials
  -> DB.Entity DB.Video
  -> [ExportSegment]
  -> AppM (Either String (DB.Key DB.Export))
takeExportAsync _ _ _ [] = do
  pure $ Left "Video segment list is empty, not taking export."
takeExportAsync clientEnv awsCredentials video exportSegments = do
  let serializedSegments =
        map (_esStartTimestamp &&& _esEndTimestamp) exportSegments
  logM ("Taking export..." :: String)
  exportId <- runSqlM $ ESQ.insert $ DB.Export Nothing 0 Nothing
  async $
    runExceptT
      (Twitch.downloadVideo
         clientEnv
         awsCredentials
         (fromDatabase (DB.entityVal video))
         serializedSegments
         ("export" <> show (ESQ.fromSqlKey exportId))
         ("exports" </> show (ESQ.fromSqlKey exportId))) >>= \case
      Left downloadError -> liftIO $ print downloadError
      Right upload_progress_chan -> do
        async $ updateUploadProgress exportId upload_progress_chan
        pure ()
  pure $ Right exportId

updateUploadProgress exportId upload_progress_chan = do
  timeout (5 # Minute) (readChan upload_progress_chan) >>= \case
    Nothing -> do
      runSqlM $
        ESQ.update $ \e -> do
          ESQ.set
            e
            [ DB.EError ESQ.=.
              ESQ.just (ESQ.val "Timed out while waiting for export updates.")
            ]
          ESQ.where_ $ e ESQ.^. DB.ExportId ESQ.==. (ESQ.val exportId)
    Just (AWS.UploadInProgress percentage) -> do
      runSqlM $
        ESQ.update $ \e -> do
          ESQ.set e [DB.ECompletion ESQ.=. ESQ.val percentage]
          ESQ.where_ $ e ESQ.^. DB.ExportId ESQ.==. (ESQ.val exportId)
      updateUploadProgress exportId upload_progress_chan
    Just (AWS.UploadFinished mp4_url) ->
      runSqlM $
      ESQ.update $ \e -> do
        ESQ.set e [DB.EUrl ESQ.=. ESQ.just (ESQ.val mp4_url)]
        ESQ.set e [DB.ECompletion ESQ.=. ESQ.val 100]
        ESQ.where_ $ e ESQ.^. DB.ExportId ESQ.==. (ESQ.val exportId)

myApi :: Proxy API
myApi = Proxy

app :: ServantMetrics -> AppMetrics -> DB.SqlCtrl -> AppCtx -> Application
app servantMetrics appMetrics sqlCtrl appCtx =
  allowCorsMiddleware $
  monitorServant (Proxy @API) servantMetrics $
  serve myApi $
  hoistServerWithContext
    myApi
    (Proxy :: Proxy '[])
    (flip runReaderT appCtx)
    server

runServer ::
     ServantMetrics
  -> AppMetrics
  -> ClientEnv
  -> AWS.Credentials
  -> Int
  -> DB.SqlCtrl
  -> LoggerSet
  -> IO ()
runServer servantMetrics appMetrics twitchClientEnv awsCredentials port sqlCtrl loggerSet = do
  let appCtx = AppCtx twitchClientEnv awsCredentials sqlCtrl loggerSet
  pushLogStrLn loggerSet $
    toLogStr $ "Server is running on port " <> show port <> "..."
  withStdoutLogger $ \logger ->
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
     in Warp.runSettings settings (app servantMetrics appMetrics sqlCtrl appCtx)

allowCorsMiddleware :: Middleware
allowCorsMiddleware = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

$(JSON.deriveJSON jsonPrefix ''TakeExportRequest)

$(JSON.deriveJSON jsonPrefix ''TakeExportResponse)

$(JSON.deriveJSON jsonPrefix ''ExportSegment)
