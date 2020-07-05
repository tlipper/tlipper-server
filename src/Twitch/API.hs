{-# LANGUAGE DataKinds #-}
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
    { _vrId :: T.Text
    , _vrUserId :: T.Text
    , _vrUserName :: T.Text
    , _vrTitle :: T.Text
    , _vrDescription :: T.Text
    , _vrCreatedAt :: T.Text
    , _vrPublishedAt :: T.Text
    , _vrUrl :: T.Text
    , _vrThumbnailUrl :: T.Text
    , _vrViewable :: T.Text
    , _vrViewCount :: Integer
    , _vrLanguage :: T.Text
    , _vrType :: T.Text
    , _vrDuration :: T.Text
    }
  deriving (Generic, Show, Eq)

data ListVideosResponse =
  ListVideosResponse
    { _lvrData :: [Video]
    }
  deriving (Generic, Show, Eq)

type ListVideos
   = "videos" :> QueryParam "user_id" Integer :> Get '[ JSON] ListVideosResponse

type API = ListVideos

api :: Proxy API
api = Proxy

listVideos :: Maybe Integer -> ClientM ListVideosResponse
listVideos = client api

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
