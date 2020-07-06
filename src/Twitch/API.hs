{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.API where

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

data ListVideosResponse =
  ListVideosResponse
    { _lvrData :: [Video]
    , _lvrPagination :: Pagination String
    }
  deriving (Show)

data Pagination cursor = Pagination { _pCursor :: Maybe cursor } deriving (Generic, Show, Eq)

listVideos :: Maybe Int -> Maybe String -> ClientM ListVideosResponse
listVideos = client api

listPaginatedVideos :: Maybe Int -> ClientM [Video]
listPaginatedVideos mbUserId =
  paginate (listVideos mbUserId) _lvrData (_pCursor . _lvrPagination)

type ListVideos
   = "videos" :> QueryParam "user_id" Int :> QueryParam "after" String :> Get '[ JSON] ListVideosResponse

type API = ListVideos

api :: Proxy API
api = Proxy

paginate
  :: forall resp result cursor m s
  . (Monad m, Semigroup s)
  => (Maybe cursor -> m resp)
  -- ^ Given an optional pagination cursor, run the request
  -> (resp -> s)
  -- ^ Pick result set from the response
  -> (resp -> Maybe cursor)
  -- ^ Pick possible pagination cursor from the response
  -> m s
  -- ^ Combined results
paginate act getResult getPaginationCursor = do
  resp <- act Nothing
  case getPaginationCursor resp of
    Nothing ->
      pure $ getResult resp
    (Just cursor) -> do
      (getResult resp <>) <$> go cursor
  where
    go :: cursor -> m s
    go cursor = do
      resp <- act (Just cursor)
      case getPaginationCursor resp of
        Nothing ->
          pure $ getResult resp
        (Just newCursor) ->
          (getResult resp <>) <$> go newCursor


twitchAPIBaseUrl :: IsString s => s
twitchAPIBaseUrl = "https://api.twitch.tv/helix/"

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
                        ]
                    }
          }
  manager <- newTlsManagerWith managerSettings
  baseUrl <- parseBaseUrl twitchAPIBaseUrl
  pure $ mkClientEnv manager baseUrl

$(JSON.deriveJSON (aesonPrefix snakeCase) ''ListVideosResponse)
$(JSON.deriveJSON (aesonPrefix snakeCase) ''Video)
$(JSON.deriveJSON (aesonPrefix snakeCase) ''Pagination)
