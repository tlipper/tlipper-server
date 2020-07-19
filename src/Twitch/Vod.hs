{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitch.Vod where

import AWS.API as AWS
import Control.Concurrent (forkIO)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.AWS as AWS
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.IO.Handle (Handle, hGetChar, hGetContents, hGetLine)
import qualified Network.AWS.S3 as AWS
import Servant.Client
import System.Exit (ExitCode(ExitSuccess))
import System.Process
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import qualified Twitch.API as Twitch

thumbnailUrlDownloadKeyRegex :: Regex
thumbnailUrlDownloadKeyRegex =
  makeRegex ("https://.+/cf_vods/.+/(.+)//thumb/.+" :: String)

downloadVideo ::
     forall m. (MonadBaseControl IO m, MonadError String m, MonadIO m)
  => AWS.Credentials
  -> ClientEnv
  -> Twitch.Video
  -> m T.Text
downloadVideo awsCredentials clientEnv video = do
  download_key <- get_video_download_key video
  let chunk_start = 1000
  let chunk_end = 1010
  mpeg_file_paths <-
    forConcurrently [chunk_start .. chunk_end] $ \chunk -> do
      liftIO $ putStrLn $ "downloading chunk " <> show chunk
      mpeg_file_path <- download_mpeg_file download_key chunk
      liftIO $ putStrLn $ "finished downloading chunk " <> show chunk
      pure mpeg_file_path
  liftIO $ putStrLn $ "merging mpeg chunks..."
  out_mp4_file_path <- merge_mpeg_files chunk_start chunk_end
  liftIO $ putStrLn $ "deleting mpeg files..."
  delete_mpeg_files mpeg_file_paths
  liftIO $ putStrLn $ "uploading mp4 file to s3..."
  upload_mp4_file_to_s3 download_key "output.mp4"
  where
    get_video_download_key :: Twitch.Video -> m String
    get_video_download_key Twitch.Video {Twitch._vThumbnailUrl} = do
      case match thumbnailUrlDownloadKeyRegex (T.unpack _vThumbnailUrl) of
        MR {mrSubList = []} ->
          throwError "Thumbnail url didn't match the regex."
        MR {mrSubList = (downloadKey:_)} -> pure downloadKey
    download_mpeg_file :: String -> Int -> m FilePath
    download_mpeg_file download_key chunk = do
      let url =
            "https://vod-secure.twitch.tv/" <>
            download_key <> "/chunked/" <> show chunk <> ".ts"
      let out_mpeg_file_path = mpeg_chunk_file_name chunk
      readProcessM
        "curl"
        ["-X", "GET", url, "--output", out_mpeg_file_path]
        Nothing
      pure out_mpeg_file_path
    mpeg_chunk_file_name :: Int -> String
    mpeg_chunk_file_name chunk = "chunk" <> show chunk <> ".mpeg"
    merge_mpeg_files :: Int -> Int -> m FilePath
    merge_mpeg_files chunk_start chunk_end = do
      let mpeg_file_paths =
            [mpeg_chunk_file_name chunk | chunk <- [chunk_start .. chunk_end]]
      let ffmpeg_input =
            "concat:" <> (T.intercalate "|" (map T.pack mpeg_file_paths))
      let out_mp4_file_path = "output.mp4"
      readProcessM
        "ffmpeg"
        ["-i", T.unpack ffmpeg_input, "-codec", "copy", out_mp4_file_path]
        Nothing
      pure out_mp4_file_path
    decode_video :: FilePath -> Int -> m FilePath
    decode_video mts_file_path chunk = do
      let out_mp4_file_path = "chunk" <> show chunk <> ".mp4"
      readProcessM "ffmpeg" ["-i", mts_file_path, out_mp4_file_path] Nothing
      pure out_mp4_file_path
    delete_mpeg_files :: (MonadError String m, MonadIO m) => [FilePath] -> m ()
    delete_mpeg_files mpeg_file_paths = do
      readProcessM "rm" mpeg_file_paths Nothing
    upload_mp4_file_to_s3 :: String -> FilePath -> m T.Text
    upload_mp4_file_to_s3 download_key file_path = do
      cmd_chan <- liftIO newChan
      stop_reading_out_chan <- liftIO newEmptyMVar
      void $
        liftIO $
        forkIO $
        fix $ \rec -> do
          tryReadMVar stop_reading_out_chan >>= \case
            Nothing -> do
              putChar =<< readChan cmd_chan
              rec
            Just _ -> do
              pure ()
      readProcessM
        "aws"
        [ "s3"
        , "mv"
        , file_path
        , "s3://twitch-vodder-mpeg/" <> download_key <> ".mp4"
        ]
        (Just cmd_chan)
      readProcessM
        "aws"
        [ "s3api"
        , "put-object-acl"
        , "--bucket"
        , "twitch-vodder-mpeg"
        , "--key"
        , download_key <> ".mp4"
        , "--acl"
        , "public-read"
        ]
        Nothing
      liftIO $ putMVar stop_reading_out_chan ()
      pure $
        "https://twitch-vodder-mpeg.s3.eu-west-2.amazonaws.com/" <>
        T.pack download_key <> ".mp4"

readProcessM ::
     (MonadError String m, MonadIO m)
  => String
  -> [String]
  -> Maybe (Chan Char)
  -> m ()
readProcessM cmd args mb_out_chan = do
  (code, err) <-
    case mb_out_chan of
      Nothing ->
        liftIO $ do
          (_, _, Just stderr_handle, p_handle) <-
            createProcess (proc cmd args) {std_err = CreatePipe}
          code <- waitForProcess p_handle
          err <- hGetContents stderr_handle
          pure (code, err)
      Just out_chan ->
        liftIO $ do
          (_, Just stdout_handle, Just stderr_handle, p_handle) <-
            createProcess
              (proc cmd args) {std_out = CreatePipe, std_err = CreatePipe}
          code <- waitForProcess p_handle
          stream_handle_to_chan stdout_handle out_chan
          err <- hGetContents stderr_handle
          pure (code, err)
  case code of
    ExitSuccess -> pure ()
    other ->
      throwError $
      "Error while executing '" <>
      cmd <> "' command: " <> show other <> ". Error output: " <> err
  where
    stream_handle_to_chan :: Handle -> Chan Char -> IO ()
    stream_handle_to_chan h chan = do
      ((Just <$> hGetChar h) `catch` \(_ :: IOError) -> pure Nothing) >>= \case
        Nothing -> pure ()
        Just !out -> writeChan chan out *> stream_handle_to_chan h chan
