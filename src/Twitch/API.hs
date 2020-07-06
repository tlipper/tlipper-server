{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.API where

import Aeson.Extra (jsonPrefix)
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

listVideos :: Maybe Int -> Maybe String -> ClientM ListVideosResponse
listClips ::
     Maybe String -> Maybe Int -> Maybe String -> ClientM ListClipsResponse
listVideos :<|> listClips = client api

listPaginatedVideos :: Maybe Int -> ClientM [Video]
listPaginatedVideos mbUserId =
  paginate
    (listVideos mbUserId)
    _lvrData
    (_pCursor . _lvrPagination)
    (const True)
    500

listPaginatedClips :: Maybe String -> (Clip -> Bool) -> ClientM [Clip]
listPaginatedClips mbUserId chunkFilter =
  paginate
    (listClips mbUserId (Just 100))
    _lcrClips
    (_lcr_Cursor)
    chunkFilter
    10000

type ListVideos
   = "helix" :> "videos" :> QueryParam "user_id" Int :> QueryParam "after" String :> Get '[ JSON] ListVideosResponse

type ListClips
   = "kraken" :> "clips" :> "top" :> QueryParam "channel" String :> QueryParam "limit" Int :> QueryParam "cursor" String :> Get '[ JSON] ListClipsResponse

type API = ListVideos :<|> ListClips

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
  -> m [value]
  -- ^ Combined results
paginate act getResult getPaginationCursor chunkFilter limit = do
  resp <- act Nothing
  case getPaginationCursor resp of
    Nothing -> pure $ take limit $ filter chunkFilter $ getResult resp
    (Just cursor) -> do
      go (filter chunkFilter (getResult resp)) cursor
  where
    go :: [value] -> cursor -> m [value]
    go results cursor = do
      if length results >= limit
        then pure $ take limit $ results
        else do
          resp <- act (Just cursor)
          let newResults = filter chunkFilter (getResult resp)
          case ( length (results <> newResults) >= limit
               , getPaginationCursor resp) of
            (False, Just newCursor)
              | cursor /= newCursor -> go (results <> newResults) newCursor
            _ -> pure $ take limit $ results <> newResults

twitchAPIBaseUrl :: IsString s => s
twitchAPIBaseUrl = "https://api.twitch.tv/"

mkTwitchClientEnv :: String -> String -> IO ClientEnv
mkTwitchClientEnv token clientId = do
  let managerSettings =
        tlsManagerSettings
          { managerModifyRequest =
              \req -> do
                print req
                putStrLn ""
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
