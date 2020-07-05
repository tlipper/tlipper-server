{-# LANGUAGE LambdaCase #-}

module Main where

import Database (PostgresqlParams(..), withDBMigration)
import Server (runServer)
import System.Environment (getEnv)
import qualified Twitch.API as Twitch

main :: IO ()
main = do
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
