{-# LANGUAGE DataKinds #-}
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
import Servant.Client (ClientEnv)
import Servant.Server
import qualified Twitch.API as Twitch
import qualified Twitch.Vod as Twitch

type API
   = "videos" :> Get '[ JSON] [DB.Video] -- GET /books
      :<|> "download" :> ReqBody '[ JSON] String :> Post '[ JSON] Twitch.Video

data ServerState =
  ServerState
    { _ssTwitchClientEnv :: ClientEnv
    , _ssVideos :: [Twitch.Video]
    }

server :: IORef ServerState -> DB.SqlCtrl -> Server API
server state_ref (DB.SqlCtrl runSql) = listVideos :<|> downloadVideo
  where
    listVideos = do
      state <- liftIO $ readIORef state_ref
      liftIO $
        fmap (map DB.entityVal) $ runSql (DB.select $ DB.from $ \v -> return v)
    downloadVideo :: String -> Handler Twitch.Video
    downloadVideo video_id = do
      state <- liftIO $ readIORef state_ref
      case find ((== video_id) . T.unpack . Twitch._vrId) (_ssVideos state) of
        Just video -> do
          ei <-
            runExceptT $ Twitch.downloadVideo (_ssTwitchClientEnv state) video
          either (\e -> throwError err400 {errBody = LBS8.pack e}) pure ei
          pure video
        Nothing -> throwError err400

myApi :: Proxy API
myApi = Proxy

app :: IORef ServerState -> DB.SqlCtrl -> Application
app state sqlCtrl = serve myApi (server state sqlCtrl)

initServerState :: ClientEnv -> ServerState
initServerState twitchClientEnv = ServerState twitchClientEnv []

runServer :: ClientEnv -> Int -> DB.SqlCtrl -> IO ()
runServer twitchClientEnv port sqlCtrl = do
  state <- newIORef (initServerState twitchClientEnv)
  Warp.run port (app state sqlCtrl)
