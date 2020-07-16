{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.API where

import Aeson.Extra (jsonPrefix)
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import Data.ByteString.Char8 (pack)
import Data.Proxy
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client (ManagerSettings(..), Request(..))
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

data Channel =
  Channel
    { _chBroadcasterLanguage :: T.Text
    , _chDisplayName :: T.Text
    , _chGameId :: T.Text
    , _chId :: T.Text
    , _chIsLive :: Bool
    , _chThumbnailUrl :: T.Text
    , _chTitle :: T.Text
    , _chStartedAt :: T.Text
    }
  deriving (Generic, Show, Eq)

data Video =
  Video
    { _vId :: T.Text
    , _vUserId :: T.Text
    , _vUserName :: T.Text
    , _vTitle :: T.Text
    , _vDescription :: T.Text
    , _vCreatedAt :: T.Text
    , _vPublishedAt :: T.Text
    , _vUrl :: T.Text
    , _vThumbnailUrl :: T.Text
    , _vViewable :: T.Text
    , _vViewCount :: Int
    , _vLanguage :: T.Text
    , _vType :: T.Text
    , _vDuration :: T.Text
    }
  deriving (Generic, Show, Eq)

data ClipVod =
  ClipVod
    { _cvId :: T.Text
    , _cvUrl :: T.Text
    }
  deriving (Generic, Show, Eq)

data Clip =
  Clip
    { _cSlug :: T.Text
    , _cTrackingId :: T.Text
    , _cUrl :: T.Text
    , _cEmbedUrl :: T.Text
    , _cEmbedHtml :: T.Text
    , _cVod :: ClipVod
    , _cGame :: T.Text
    , _cLanguage :: T.Text
    , _cTitle :: T.Text
    , _cViews :: Int
    , _cDuration :: Double
    , _cCreatedAt :: T.Text
    }
  deriving (Generic, Show, Eq)

data ListVideosResponse =
  ListVideosResponse
    { _lvrData :: [Video]
    , _lvrPagination :: Pagination String
    }
  deriving (Show)

data SearchChannelsResponse =
  SearchChannelsResponse
    { _scrData :: [Channel]
    , _scrPagination :: Pagination String
    }
  deriving (Show)

data ListClipsResponse =
  ListClipsResponse
    { _lcrClips :: [Clip]
    , _lcr_Cursor :: Maybe String
    }
  deriving (Show)

data Pagination cursor =
  Pagination
    { _pCursor :: Maybe cursor
    }
  deriving (Generic, Show, Eq)

searchChannels :: T.Text -> Maybe String -> ClientM SearchChannelsResponse
listVideos :: Maybe Int -> Maybe String -> ClientM ListVideosResponse
listClips ::
     Maybe String -> Maybe Int -> Maybe String -> ClientM ListClipsResponse
searchChannels :<|> listVideos :<|> listClips = client api

searchPaginatedChannels :: T.Text -> ClientM [Channel]
searchPaginatedChannels channelName =
  paginate
    (searchChannels channelName)
    _scrData
    (_pCursor . _scrPagination)
    (const True)
    500
    (const (pure ()))

listPaginatedVideos :: Maybe Int -> ClientM [Video]
listPaginatedVideos mbUserId =
  paginate
    (listVideos mbUserId)
    _lvrData
    (_pCursor . _lvrPagination)
    (const True)
    500
    (const (pure ()))

listPaginatedClips :: Maybe String -> (Clip -> Bool) -> ClientM [Clip]
listPaginatedClips mbUserId chunkFilter =
  paginate
    (listClips mbUserId (Just 100))
    _lcrClips
    (_lcr_Cursor)
    chunkFilter
    100000
    (liftIO . print)

type SearchChannels
   = "helix" :> "search" :> "channels" :> QueryParam' '[ Required] "query" T.Text :> QueryParam "after" String :> Get '[ JSON] SearchChannelsResponse

type ListVideos
   = "helix" :> "videos" :> QueryParam "user_id" Int :> QueryParam "after" String :> Get '[ JSON] ListVideosResponse

type ListClips
   = "kraken" :> "clips" :> "top" :> QueryParam "channel" String :> QueryParam "limit" Int :> QueryParam "cursor" String :> Get '[ JSON] ListClipsResponse

type API = SearchChannels :<|> ListVideos :<|> ListClips

api :: Proxy API
api = Proxy

paginate ::
     forall resp result cursor m value. (Monad m, Eq cursor)
  => (Maybe cursor -> m resp)
  -- ^ Given an optional pagination cursor, run the request
  -> (resp -> [value])
  -- ^ Pick result set from the response
  -> (resp -> Maybe cursor)
  -- ^ Pick possible pagination cursor from the response
  -> (value -> Bool)
  -- ^ Filter for the chunks of paginated data
  -> Int
  -- ^ Limit the overall results.
  -> (Int -> m ())
  -- ^ Do some action with the page number.
  -> m [value]
  -- ^ Combined results
paginate act getResult getPaginationCursor chunkFilter limit sideEffect = do
  resp <- act Nothing
  case getPaginationCursor resp of
    Nothing -> pure $ take limit $ filter chunkFilter $ getResult resp
    (Just cursor) -> do
      go 1 (filter chunkFilter (getResult resp)) cursor
  where
    go :: Int -> [value] -> cursor -> m [value]
    go page results cursor = do
      sideEffect page
      if length results >= limit
        then pure $ take limit $ results
        else do
          resp <- act (Just cursor)
          let newResults = filter chunkFilter (getResult resp)
          case ( length (results <> newResults) >= limit
               , getPaginationCursor resp) of
            (False, Just newCursor)
              | cursor /= newCursor ->
                go (page + 1) (results <> newResults) newCursor
            _ -> pure $ take limit $ results <> newResults

twitchAPIBaseUrl :: IsString s => s
twitchAPIBaseUrl = "https://api.twitch.tv/"

mkTwitchClientEnv :: String -> String -> IO ClientEnv
mkTwitchClientEnv token clientId = do
  let managerSettings =
        tlsManagerSettings
          { managerModifyRequest =
              \req ->
                pure
                  req
                    { requestHeaders =
                        [ ("Authorization", pack ("Bearer " <> token))
                        , ("Client-ID", pack clientId)
                        , ("Accept", "application/vnd.twitchtv.v5+json")
                        ]
                    }
          }
  manager <- newTlsManagerWith managerSettings
  baseUrl <- parseBaseUrl twitchAPIBaseUrl
  pure $ mkClientEnv manager baseUrl

$(JSON.deriveJSON jsonPrefix ''ListVideosResponse)

$(JSON.deriveJSON jsonPrefix ''ListClipsResponse)

$(JSON.deriveJSON jsonPrefix ''Video)

$(JSON.deriveJSON jsonPrefix ''ClipVod)

$(JSON.deriveJSON jsonPrefix ''Clip)

$(JSON.deriveJSON jsonPrefix ''Pagination)

$(JSON.deriveJSON jsonPrefix ''Channel)

$(JSON.deriveJSON jsonPrefix ''SearchChannelsResponse)
