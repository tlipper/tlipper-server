{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitch.Vod where

import Control.Concurrent.Async.Lifted
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Data.Traversable (for)
import Servant.Client
import System.Exit (ExitCode(ExitSuccess))
import System.Process
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Twitch.API

thumbnailUrlDownloadKeyRegex :: Regex
thumbnailUrlDownloadKeyRegex =
  makeRegex ("https://.+/cf_vods/.+/(.+)//thumb/.+" :: String)

downloadVideo ::
     forall m. (MonadBaseControl IO m, MonadError String m, MonadIO m)
  => ClientEnv
  -> Video
  -> m ()
downloadVideo clientEnv video = do
  download_key <- get_video_download_key video
  mpeg_file_paths <-
    forConcurrently [1000 .. 1100] $ \chunk -> do
      liftIO $ putStrLn $ "doing chunk " <> show chunk
      download_mpeg_file download_key chunk
  _ <- merge_mpeg_files mpeg_file_paths
  delete_mpeg_files mpeg_file_paths
  where
    get_video_download_key :: Video -> m String
    get_video_download_key Video {_vrThumbnailUrl} = do
      case match thumbnailUrlDownloadKeyRegex (T.unpack _vrThumbnailUrl) of
        MR {mrSubList = []} ->
          throwError "Thumbnail url didn't match the regex."
        MR {mrSubList = (downloadKey:_)} -> pure downloadKey
    download_mpeg_file :: String -> Int -> m FilePath
    download_mpeg_file download_key chunk = do
      let url =
            "https://vod-secure.twitch.tv/" <>
            download_key <> "/chunked/" <> show chunk <> ".ts"
      let out_mpeg_file_path = "chunk" <> show chunk <> ".mpeg"
      readProcessM "curl" ["-X", "GET", url, "--output", out_mpeg_file_path]
      pure out_mpeg_file_path
    merge_mpeg_files :: [FilePath] -> m FilePath
    merge_mpeg_files mpeg_file_paths = do
      let ffmpeg_input =
            "concat:" <> (T.intercalate "|" (map T.pack mpeg_file_paths))
      let out_mp4_file_path = "output.mp4"
      readProcessM
        "ffmpeg"
        ["-i", T.unpack ffmpeg_input, "-codec", "copy", out_mp4_file_path]
      pure out_mp4_file_path
    decode_video :: FilePath -> Int -> m FilePath
    decode_video mts_file_path chunk = do
      let out_mp4_file_path = "chunk" <> show chunk <> ".mp4"
      readProcessM "ffmpeg" ["-i", mts_file_path, out_mp4_file_path]
      pure out_mp4_file_path
    delete_mpeg_files :: (MonadError String m, MonadIO m) => [FilePath] -> m ()
    delete_mpeg_files mpeg_file_paths = do
      readProcessM "rm" mpeg_file_paths

readProcessM :: (MonadError String m, MonadIO m) => String -> [String] -> m ()
readProcessM cmd args = do
  (code, out, err) <- liftIO $ readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> pure ()
    other ->
      throwError $
      "Error while executing '" <>
      cmd <> "' command: " <> show other <> ". Error output: " <> err
