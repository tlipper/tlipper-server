{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (find)
import Data.IORef
import qualified Data.Text as T
import qualified Database as DB
import qualified Database.Esqueleto as DB
import qualified Database.Persist as DB
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client (ClientEnv, runClientM)
import Data.Foldable (for_)
import Servant.Server
import qualified Twitch.API as Twitch
import qualified Twitch.Vod as Twitch
import qualified Database as DB
import Database.Middleware

type VideoId = T.Text
type UserId = Int

type API
     = "videos" :> Get '[ JSON] [Twitch.Video]
  :<|> "download" :> ReqBody '[ JSON] VideoId :> Post '[ JSON] Twitch.Video
  :<|> "videos" :> "sync" :> ReqBody '[ JSON] UserId :> Post '[ JSON] ()

data ServerState =
  ServerState
    { _ssTwitchClientEnv :: ClientEnv
    }

server :: IORef ServerState -> DB.SqlCtrl -> Server API
server state_ref (DB.SqlCtrl runSql) = listVideos :<|> downloadVideo :<|> syncVideos
  where
    listVideos :: Handler [Twitch.Video]
    listVideos = do
      state <- liftIO $ readIORef state_ref
      dbVideos <- liftIO $
        fmap (map DB.entityVal) $ runSql (DB.select $ DB.from $ \v -> return v)
      pure $ map fromDatabase dbVideos
    downloadVideo :: VideoId -> Handler Twitch.Video
    downloadVideo video_id = do
      state <- liftIO $ readIORef state_ref
      videos <- liftIO $ fmap (map DB.entityVal) $ runSql (DB.select $ DB.from $ \v -> return v)
      case find ((== video_id) . DB.vVid) videos of
        Just (fromDatabase -> video) -> do
          ei <-
            runExceptT $ Twitch.downloadVideo (_ssTwitchClientEnv state) video
          either (\e -> throwError err400 {errBody = LBS8.pack e}) pure ei
          pure video
        Nothing -> throwError err400
    syncVideos :: UserId -> Handler ()
    syncVideos user_id = do
      state <- liftIO $ readIORef state_ref
      let clientEnv = _ssTwitchClientEnv state
      liftIO (runClientM (Twitch.listPaginatedVideos (Just user_id)) clientEnv) >>= \case
        Left clientErr ->
          throwError err500 { errBody = LBS8.pack (show clientErr) }
        Right videos ->
          liftIO $ runSql $ DB.putMany $ map toDatabase videos

myApi :: Proxy API
myApi = Proxy

app :: IORef ServerState -> DB.SqlCtrl -> Application
app state sqlCtrl = serve myApi (server state sqlCtrl)

initServerState :: ClientEnv -> ServerState
initServerState twitchClientEnv = ServerState twitchClientEnv

runServer :: ClientEnv -> Int -> DB.SqlCtrl -> IO ()
runServer twitchClientEnv port sqlCtrl = do
  state <- newIORef (initServerState twitchClientEnv)
  Warp.run port (app state sqlCtrl)
