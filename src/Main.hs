{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Logger.CallStack
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor (void)
import Database (PostgresqlParams(..), withDBMigration)
import Monitoring (HttpMetrics(..), withPrometheus)
import Server (runServer)
import System.Environment (getEnv)
import System.Log.FastLogger
import qualified Twitch.API as Twitch

main :: IO ()
main = do
  void $ loadFile defaultConfig
  awsCredentials <-
    AWS.FromKeys <$> (AWS.AccessKey . BS8.pack <$> getEnv "AWS_ACCESS_KEY") <*>
    (AWS.SecretKey . BS8.pack <$> getEnv "AWS_SECRET_KEY")
  twitchAppAccessToken <- getEnv "TWITCH_APP_ACCESS_TOKEN"
  twitchClientId <- getEnv "TWITCH_CLIENT_ID"
  -- serverPort <- read <$> getEnv "SERVER_PORT"
  dbHost <- getEnv "DB_HOST"
  dbPort <- read <$> getEnv "DB_PORT"
  dbUser <- getEnv "DB_USER"
  dbName <- getEnv "DB_NAME"
  dbPass <- getEnv "DB_PASS"
  let postgresqlParams = PostgresqlParams dbHost dbPort dbUser dbName dbPass
  loggerSet <- newStdoutLoggerSet 100
  twitchClientEnv <-
    Twitch.mkTwitchClientEnv twitchAppAccessToken twitchClientId
  withPrometheus $ \servantMetrics counters -> do
    withDBMigration postgresqlParams $ \sqlCtrl -> do
      runServer
        servantMetrics
        counters
        twitchClientEnv
        awsCredentials
        8080
        sqlCtrl
        loggerSet
