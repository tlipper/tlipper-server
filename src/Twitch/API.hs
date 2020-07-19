{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Maybe (mapMaybe)
import Data.Proxy
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
import Debug.Trace (trace)
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
  deriving (Generic, Show, Eq, Ord)

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
  deriving (Generic, Show, Eq, Ord)

data ClipVod =
  ClipVod
    { _cvId :: T.Text
    , _cvUrl :: T.Text
    }
  deriving (Generic, Show, Eq, Ord)

data Clip' vod =
  Clip
    { _cSlug :: T.Text
    , _cTrackingId :: T.Text
    , _cUrl :: T.Text
    , _cEmbedUrl :: T.Text
    , _cEmbedHtml :: T.Text
    , _cVod :: vod
    , _cGame :: T.Text
    , _cLanguage :: T.Text
    , _cTitle :: T.Text
    , _cViews :: Int
    , _cDuration :: Double
    , _cCreatedAt :: T.Text
    }
  deriving (Generic, Show, Eq, Ord)

type Clip = Clip' ClipVod

type ResponseClip = Clip' (Maybe ClipVod)

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
    { _lcrClips :: [ResponseClip]
    , _lcr_Cursor :: Maybe String
    }
  deriving (Show)

data Pagination cursor =
  Pagination
    { _pCursor :: Maybe cursor
    }
  deriving (Generic, Show, Eq)

searchChannels :: T.Text -> Maybe String -> ClientM SearchChannelsResponse
listVideos ::
     Maybe Int -> Maybe T.Text -> Maybe String -> ClientM ListVideosResponse
listClips ::
     Maybe String
  -> Maybe Int
  -> Maybe String
  -> Maybe String
  -> ClientM ListClipsResponse
searchChannels :<|> listVideos :<|> listClips = client api

filterOutNonVodClips :: [ResponseClip] -> [Clip]
filterOutNonVodClips =
  mapMaybe $ \c ->
    case _cVod c of
      Nothing -> Nothing
      Just vod -> Just $ c {_cVod = vod}

searchPaginatedChannels :: T.Text -> ClientM (Set.Set Channel)
searchPaginatedChannels channelName =
  paginate
    (searchChannels channelName)
    _scrData
    (_pCursor . _scrPagination)
    (const True)
    (_chId)
    500
    (\_ _ -> pure ())

listPaginatedVideos :: Maybe T.Text -> ClientM (Set.Set Video)
listPaginatedVideos mbChannelId =
  paginate
    (listVideos (Just 100) mbChannelId)
    _lvrData
    (_pCursor . _lvrPagination)
    (const True)
    (_vId)
    10000
    (\p r ->
       liftIO $
       putStrLn $ "[Videos] Page: " <> show p <> ", Results: " <> show r)

listPaginatedClips :: Maybe String -> (Clip -> Bool) -> ClientM (Set.Set Clip)
listPaginatedClips mbUserName chunkFilter = do
  liftIO $ putStrLn "paginating..."
  paginate
    (listClips mbUserName (Just 100) (Just "all"))
    (filterOutNonVodClips . _lcrClips)
    (_lcr_Cursor)
    chunkFilter
    (_cTrackingId)
    1000000
    (\p r ->
       liftIO $ putStrLn $ "[Clips] Page: " <> show p <> ", Results: " <> show r)

type SearchChannels
   = "helix" :> "search" :> "channels" :> QueryParam' '[ Required] "query" T.Text :> QueryParam "after" String :> Get '[ JSON] SearchChannelsResponse

type ListVideos
   = "helix" :> "videos" :> QueryParam "first" Int :> QueryParam "user_id" T.Text :> QueryParam "after" String :> Get '[ JSON] ListVideosResponse

type ListClips
   = "kraken" :> "clips" :> "top" :> QueryParam "channel" String :> QueryParam "limit" Int :> QueryParam "period" String :> QueryParam "cursor" String :> Get '[ JSON] ListClipsResponse

type API = SearchChannels :<|> ListVideos :<|> ListClips

api :: Proxy API
api = Proxy

paginate ::
     forall resp result cursor m value key.
     (Monad m, Eq cursor, Eq key, Ord key, Ord value, Show value)
  => (Maybe cursor -> m resp)
  -- ^ Given an optional pagination cursor, run the request
  -> (resp -> [value])
  -- ^ Pick result set from the response
  -> (resp -> Maybe cursor)
  -- ^ Pick possible pagination cursor from the response
  -> (value -> Bool)
  -- ^ Filter for the chunks of paginated data
  -> (value -> key)
  -- ^ Unique identifier for paginated data
  -> Int
  -- ^ Limit the overall results.
  -> (Int -> Int -> m ())
  -- ^ Do some action with the page number & result length.
  -> m (Set.Set value)
  -- ^ Combined results
paginate act getResult getPaginationCursor chunkFilter uniqueIdentifier limit sideEffect = do
  resp <- act Nothing
  case getPaginationCursor resp of
    Nothing -> do
      let results =
            Set.take limit $
            Set.filter chunkFilter $ Set.fromList $ getResult resp
      sideEffect (-1) (length results)
      pure results
    (Just cursor) -> do
      let filteredResults =
            Set.filter chunkFilter $ Set.fromList $ getResult resp
      let keys = Set.map uniqueIdentifier filteredResults
      go 1 filteredResults keys cursor
  where
    go :: Int -> Set.Set value -> Set.Set key -> cursor -> m (Set.Set value)
    go page results keys cursor = do
      sideEffect page (length results)
      if | Set.size results >= limit -> pure $ Set.take limit $ results
         | otherwise ->
           do resp <- act (Just cursor)
              let newResults =
                    Set.filter chunkFilter $ Set.fromList $ getResult resp
              if newResults `Set.isSubsetOf` results
                then pure results
                else do
                  case ( length (results <> newResults) >= limit
                       , getPaginationCursor resp) of
                    (False, Just newCursor)
                      | cursor /= newCursor ->
                        go
                          (page + 1)
                          (results <> newResults)
                          (keys <> Set.map uniqueIdentifier newResults)
                          newCursor
                    _ -> pure $ Set.take limit $ results <> newResults

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

$(JSON.deriveJSON jsonPrefix ''Clip')

$(JSON.deriveJSON jsonPrefix ''Pagination)

$(JSON.deriveJSON jsonPrefix ''Channel)

$(JSON.deriveJSON jsonPrefix ''SearchChannelsResponse)
