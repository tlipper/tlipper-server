{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Server.API where

import Data.Int
import Data.Proxy
import qualified Data.Text as T
import qualified Database as DB
import Network.HTTP.Types (Method)
import Servant
import Server.API.Endpoints (getEndpoints)
import qualified Twitch.API as Twitch
import qualified Twitch.Analytics as Twitch

type UserId = Int

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
    { _terExportId :: DB.Key DB.Export -- It's a very bad idea to couple DB type in response type. Refactor this.
    }
  deriving (Show)

type VideoId = T.Text

type API
   = ListChannels :<|> AddChannel :<|> ListVideos :<|> SyncVideo :<|> ListClips :<|> SyncClips :<|> GetVideoAnalysis :<|> ListExports :<|> GetExport :<|> TakeExport

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

type SyncVideo
   = "videos" :> "sync" :> QueryParam' '[ Required] "channel_id" T.Text :> Post '[ JSON] ()

type ListExports = "exports" :> Get '[ JSON] [DB.Export]

type GetExport
   = "exports" :> Capture' '[ Required] "export_id" Int64 :> Get '[ JSON] DB.Export

type TakeExport
   = "exports" :> ReqBody '[ JSON] TakeExportRequest :> Post '[ JSON] TakeExportResponse

endpoints :: [([T.Text], Method)]
endpoints = getEndpoints (Proxy @API)
