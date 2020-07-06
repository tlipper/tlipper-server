{-# LANGUAGE LambdaCase #-}

module Main where

import Database (PostgresqlParams(..), withDBMigration)
import Server (runServer)
import System.Environment (getEnv)
import qualified Twitch.API as Twitch
import Data.Functor (void)
import Configuration.Dotenv

main :: IO ()
main = do
  void $ loadFile defaultConfig
  awsAccessKey <- getEnv "AWS_ACCESS_KEY"
  awsSecretKey <- getEnv "AWS_SECRET_KEY"
  twitchAppAccessToken <- getEnv "TWITCH_APP_ACCESS_TOKEN"
  twitchClientId <- getEnv "TWITCH_CLIENT_ID"
  serverPort <- read <$> getEnv "SERVER_PORT"
  dbHost <- getEnv "DB_HOST"
  dbPort <- read <$> getEnv "DB_PORT"
  dbUser <- getEnv "DB_USER"
  dbName <- getEnv "DB_NAME"
  dbPass <- getEnv "DB_PASS"
  let postgresqlParams = PostgresqlParams dbHost dbPort dbUser dbName dbPass
  twitchClientEnv <-
    Twitch.mkTwitchClientEnv twitchAppAccessToken twitchClientId
  withDBMigration postgresqlParams $ \sqlCtrl -> do
    runServer twitchClientEnv serverPort sqlCtrl
