{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Arrow ((&&&), (>>>))
import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import Data.ByteString.Char8 (pack)
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
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

paginateRaw ::
     forall result cursor e t m value key.
     (Monad m, Eq cursor, Ord value, Show value)
  => (Maybe cursor -> m (Maybe cursor, [value]))
  -- ^ Given an optional pagination cursor, run the request
  -> Int
  -- ^ Limit the overall results.
  -> (Int -> Set.Set value -> m ())
  -- ^ Do some action with the page number & results.
  -> m ()
  -- ^ Combined results
paginateRaw act limit consumeResults = do
  (mb_cursor, values) <- act Nothing
  let results = Set.take limit $ Set.fromList values
  consumeResults (-1) results
  case mb_cursor of
    Nothing -> do
      pure ()
    (Just cursor) -> do
      go 1 (Set.size results) cursor
  where
    go :: Int -> Int -> cursor -> m ()
    go page resultSize cursor = do
      if | resultSize >= limit -> pure ()
         | otherwise ->
           do (mb_cursor, values) <- act (Just cursor)
              let newResults = Set.fromList values
              consumeResults page newResults
              case mb_cursor of
                (Just newCursor)
                  | cursor /= newCursor ->
                    go (page + 1) (resultSize + Set.size newResults) newCursor
                Nothing -> pure ()

class Paginatable m resource where
  type Extras resource
  type Cursor resource
  fetch ::
       forall cursor.
       Extras resource
    -> Maybe (Cursor resource)
    -> m (Maybe (Cursor resource), [resource])
  paginate ::
       Extras resource -> Int -> (Int -> Set.Set resource -> m ()) -> m ()

instance Paginatable ClientM Channel where
  type Extras Channel = T.Text -- Channel name
  type Cursor Channel = String
  fetch channel_name mb_cursor = do
    resp <- searchChannels channel_name mb_cursor
    pure $ (_pCursor . _scrPagination &&& _scrData) resp
  paginate = paginateRaw . fetch

instance Paginatable ClientM Video where
  type Extras Video = Maybe T.Text -- Maybe channel id
  type Cursor Video = String
  fetch mb_channel_id mb_cursor = do
    resp <- listVideos (Just 100) mb_channel_id mb_cursor
    pure $ (_pCursor . _lvrPagination &&& _lvrData) resp
  paginate = paginateRaw . fetch

instance Paginatable ClientM (Clip' ClipVod) where
  type Extras Clip = Maybe String -- Maybe user name
  type Cursor Clip = String
  fetch mb_user_name mb_cursor = do
    fmap mconcat $
      forConcurrently ["day", "week", "all"] $ \window -> do
        resp <- listClips mb_user_name (Just 100) (Just window) mb_cursor
        pure $ (_lcr_Cursor &&& filterOutNonVodClips . _lcrClips) resp
  paginate = paginateRaw . fetch

paginateBlocking ::
     (MonadIO m, Paginatable m resource, Ord resource)
  => Extras resource
  -> Int
  -> m (Set.Set resource)
paginateBlocking extras limit = do
  results_ref <- liftIO $ newIORef Set.empty
  paginate extras limit $ \_page results ->
    liftIO $ atomicModifyIORef' results_ref $ \s -> (s <> results, ())
  liftIO $ readIORef results_ref

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

searchPaginatedChannels ::
     T.Text -> (Int -> Set.Set Channel -> ClientM ()) -> ClientM ()
searchPaginatedChannels channelName consumeResults =
  paginateRaw
    -- First fmap is for arrow functor ((->) a), second one is for ClientM. Sorry reader.
    (fmap (_pCursor . _scrPagination &&& _scrData) <$>
     searchChannels channelName)
    500
    consumeResults

listPaginatedVideos ::
     Maybe T.Text -> (Int -> Set.Set Video -> ClientM ()) -> ClientM ()
listPaginatedVideos mbChannelId consumeResults =
  paginateRaw
    (fmap (_pCursor . _lvrPagination &&& _lvrData) <$>
     listVideos (Just 100) mbChannelId)
    10000
    (\p r -> do
       liftIO $
         putStrLn $
         "[Videos] Page: " <> show p <> ", Results: " <> show (Set.size r)
       consumeResults p r)

listPaginatedClips ::
     Maybe String
  -> (Clip -> Bool)
  -> (Int -> Set.Set Clip -> ClientM ())
  -> ClientM ()
listPaginatedClips mbUserName chunkFilter consumeResults = do
  liftIO $ putStrLn "paginating..."
  fmap mconcat $
    forConcurrently ["day", "week", "all"] $ \window ->
      paginateRaw
        (fmap (_lcr_Cursor &&& filterOutNonVodClips . _lcrClips) <$>
         listClips mbUserName (Just 100) (Just window))
        1000
        (\p r -> do
           liftIO $
             putStrLn $
             "[" <>
             window <>
             " Clips of " <>
             fromMaybe "<unknown>" mbUserName <>
             "] Page: " <> show p <> ", Results: " <> show r
           consumeResults p r)

type SearchChannels
   = "helix" :> "search" :> "channels" :> QueryParam' '[ Required] "query" T.Text :> QueryParam "after" String :> Get '[ JSON] SearchChannelsResponse

type ListVideos
   = "helix" :> "videos" :> QueryParam "first" Int :> QueryParam "user_id" T.Text :> QueryParam "after" String :> Get '[ JSON] ListVideosResponse

type ListClips
   = "kraken" :> "clips" :> "top" :> QueryParam "channel" String :> QueryParam "limit" Int :> QueryParam "period" String :> QueryParam "cursor" String :> Get '[ JSON] ListClipsResponse

type API = SearchChannels :<|> ListVideos :<|> ListClips

api :: Proxy API
api = Proxy

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
