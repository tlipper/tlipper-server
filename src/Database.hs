{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Aeson.Extra (jsonPrefix)
import Conduit
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Database.Extra
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

share
  [mkPersist appSqlSettings, mkMigrate "migrateAll"]
  [persistUnderscored|
Channel
    chBroadcasterLanguage T.Text
    chDisplayName T.Text
    chGameId T.Text
    chChId T.Text
    chIsLive Bool
    chThumbnailUrl T.Text
    chTitle T.Text
    chStartedAt T.Text
    UniqueChannelId chChId

Video
    vTwitchVideoId T.Text
    vUserId T.Text
    vUserName T.Text
    vTitle T.Text
    vDescription T.Text
    vCreatedAt T.Text
    vPublishedAt T.Text
    vUrl T.Text
    vThumbnailUrl T.Text
    vViewable T.Text
    vViewCount Int
    vLanguage T.Text
    vType T.Text
    vDuration T.Text
    UniqueTwitchVideoId vTwitchVideoId
    deriving Show

Clip
    cSlug T.Text
    cTrackingId T.Text
    cUrl T.Text
    cEmbedUrl T.Text
    cEmbedHtml T.Text
    cVodId T.Text
    cVodUrl T.Text
    cGame T.Text
    cLanguage T.Text
    cTitle T.Text
    cViews Int
    cDuration Double
    cCreatedAt T.Text
    UniqueClipId cTrackingId

VideoUrl
    vuVideoId VideoId
    vuUrl T.Text
    UniqueVideoUrlVideoId vuVideoId

Export
    eUrl T.Text Maybe
    eCompletion Int default=0
|]

data PostgresqlParams =
  PostgresqlParams
    { _ppHost :: String
    , _ppPort :: Int
    , _ppUser :: String
    , _ppName :: String
    , _ppPass :: String
    }

mkPostgresqlConnUrl :: PostgresqlParams -> String
mkPostgresqlConnUrl (PostgresqlParams host port user dbname pass) =
  "host=" <>
  host <>
  " port=" <>
  show port <> " user=" <> user <> " dbname=" <> dbname <> " password=" <> pass

newtype SqlCtrl =
  SqlCtrl
    { runSql :: forall a. (ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a)
    }

withDBMigration :: PostgresqlParams -> (SqlCtrl -> IO ()) -> IO ()
withDBMigration postgresqlParams action = do
  runNoLoggingT $
    withPostgresqlPool (BS8.pack (mkPostgresqlConnUrl postgresqlParams)) 10 $ \pool -> do
      liftIO $ flip runSqlPersistMPool pool $ do runMigration migrateAll
      liftIO $ action (SqlCtrl (flip runSqlPersistMPool pool))

$(JSON.deriveJSON jsonPrefix ''Export)
