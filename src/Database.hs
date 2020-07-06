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

import Conduit
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Char8 as BS8
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Extra
import qualified Data.Text as T

share
  [mkPersist appSqlSettings, mkMigrate "migrateAll"]
  [persistUnderscored|
Video
    vVid T.Text
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
    UniqueVideoId vVid
    deriving Show
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
  runStderrLoggingT $
    withPostgresqlPool (BS8.pack (mkPostgresqlConnUrl postgresqlParams)) 10 $ \pool -> do
      liftIO $ flip runSqlPersistMPool pool $ do runMigration migrateAll
      liftIO $ action (SqlCtrl (flip runSqlPersistMPool pool))

$(JSON.deriveJSON (aesonPrefix snakeCase) ''Video)
