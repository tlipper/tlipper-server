{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Control.Concurrent.Chan.ReadOnly
import Control.Concurrent.Chan.WriteOnly
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Lens.Combinators (TraversableWithIndex, ifor)
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.AWS as AWS
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.IO.Handle (Handle, hGetChar, hGetContents, hGetLine)
import qualified Network.AWS.S3 as AWS
import Servant.Client
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process
import Text.Printf
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import qualified Twitch.API as Twitch

thumbnailUrlDownloadKeyRegex :: Regex
thumbnailUrlDownloadKeyRegex =
  makeRegex ("https://.+/cf_vods/.+/(.+)//thumb/.+" :: String)

concurrentIFor ::
     (MonadBaseControl IO m, TraversableWithIndex i t, Applicative m)
  => t a
  -> (i -> a -> m b)
  -> m (t b)
concurrentIFor l f = runConcurrently $ ifor l $ \i a -> Concurrently (f i a)

type MonadCleanup = MonadWriter (Set.Set FilePath)

downloadVideo ::
     (MonadBaseControl IO m, MonadError String m, MonadIO m)
  => ClientEnv
  -> AWS.Credentials
  -> Twitch.Video
  -> [(Int, Int)]
  -> FilePath
  -> FilePath
  -> m (ReadOnlyChan AWS.UploadProgress)
downloadVideo clientEnv awsCredentials video segments uniqueVideoKey outDir = do
  (out_mp4_file_path, files_to_clean) <-
    runWriterT $ do
      mp4_segments <-
        concurrentIFor segments $ downloadVideoSegment clientEnv video outDir
      liftIO $ putStrLn $ "concatting mp4 files..."
      out_mp4_file_path <- concat_mp4_files mp4_segments outDir "final.mp4"
      tell $ Set.fromList $ map (outDir </>) mp4_segments
      pure out_mp4_file_path
  liftIO $ putStrLn $ "cleaning up..."
  liftIO $ print uniqueVideoKey
  readProcessM "rm" (Set.toList files_to_clean) Nothing
  update_chan <- liftIO newChan
  upload_mp4_file_to_s3
    "twitch-vodder-mp4"
    uniqueVideoKey
    out_mp4_file_path
    (toWriteOnlyChan update_chan)
  pure $ toReadOnlyChan update_chan
  where
    concat_mp4_files ::
         (MonadIO m, MonadCleanup m, MonadError String m)
      => [FilePath]
      -> FilePath
      -> FilePath
      -> m FilePath
    concat_mp4_files input_paths out_dir out_name = do
      let input_file_content =
            T.unpack $
            T.intercalate "\n" $ map (("file " <>) . T.pack) input_paths
      let input_fp = out_dir </> "input.txt"
      liftIO $ writeFile input_fp input_file_content
      let out_mp4_file_path = out_dir </> out_name
      readProcessM
        "ffmpeg"
        ["-f", "concat", "-i", input_fp, "-codec", "copy", out_mp4_file_path]
        Nothing
      tell $ Set.singleton input_fp
      pure out_mp4_file_path
    upload_mp4_file_to_s3 ::
         (MonadIO m, MonadError String m)
      => T.Text
      -> String
      -> FilePath
      -> (WriteOnlyChan AWS.UploadProgress)
      -> m ()
    upload_mp4_file_to_s3 bucket_name download_key file_path update_chan = do
      liftIO $
        AWS.putChunkedFile
          awsCredentials
          (AWS.BucketName bucket_name)
          (AWS.ObjectKey ((T.pack uniqueVideoKey) <> ".mp4"))
          Nothing
          file_path
          update_chan

downloadVideoSegment ::
     forall m.
     (MonadCleanup m, MonadBaseControl IO m, MonadError String m, MonadIO m)
  => ClientEnv
  -> Twitch.Video
  -> FilePath
  -> Int
  -> (Int, Int)
  -- | (mp4_segment_file, used_mpeg_files)
  -> m FilePath
downloadVideoSegment clientEnv video outDir segmentId (chunk_start, chunk_end) = do
  download_key <- get_video_download_key video
  let chunks =
        [floor (fromIntegral chunk_start / 10) .. floor
                                                    (fromIntegral chunk_end / 10)]
  -- Every chunk represents a 10-second portion of the video.
  mpeg_file_paths <-
    forConcurrently chunks $ \chunk -> do
      liftIO $ putStrLn $ "downloading chunk " <> show chunk
      mpeg_file_path <- download_mpeg_file outDir download_key chunk
      liftIO $ putStrLn $ "finished downloading chunk " <> show chunk
      pure mpeg_file_path
  liftIO $ putStrLn $ "merging mpeg chunks..."
  in_mp4_file_path <-
    merge_mpeg_files chunks outDir ("segment" <> show segmentId <> ".mp4")
  let out_file_name = "segment" <> show segmentId <> "_trimmed.mp4"
  let out_mp4_file_path = outDir </> out_file_name
  liftIO $ putStrLn $ "trimming mp4 file..."
  trim_mp4_video
    in_mp4_file_path
    out_mp4_file_path
    (chunk_start `mod` 10)
    (chunk_end - chunk_start)
  tell $ Set.singleton in_mp4_file_path
  liftIO $ putStrLn $ "trimming done..."
  tell $ Set.fromList mpeg_file_paths
  pure out_file_name
  where
    get_video_download_key :: Twitch.Video -> m String
    get_video_download_key Twitch.Video {Twitch._vThumbnailUrl} = do
      case match thumbnailUrlDownloadKeyRegex (T.unpack _vThumbnailUrl) of
        MR {mrSubList = []} ->
          throwError "Thumbnail url didn't match the regex."
        MR {mrSubList = (downloadKey:_)} -> pure downloadKey
    download_mpeg_file :: FilePath -> String -> Int -> m FilePath
    download_mpeg_file outDir download_key chunk = do
      liftIO $ createDirectoryIfMissing True outDir
      let url =
            "https://vod-secure.twitch.tv/" <>
            download_key <> "/chunked/" <> show chunk <> ".ts"
      let out_mpeg_file_path = outDir </> mpeg_chunk_file_name chunk
      readProcessM
        "curl"
        ["-X", "GET", url, "--output", out_mpeg_file_path]
        Nothing
      pure out_mpeg_file_path
    mpeg_chunk_file_name :: Int -> String
    mpeg_chunk_file_name chunk = "chunk" <> show chunk <> ".mpeg"
    merge_mpeg_files :: [Int] -> FilePath -> String -> m FilePath
    merge_mpeg_files chunks outDir outFileName = do
      let mpeg_file_paths =
            [outDir </> mpeg_chunk_file_name chunk | chunk <- chunks]
      let ffmpeg_input =
            "concat:" <> (T.intercalate "|" (map T.pack mpeg_file_paths))
      let out_mp4_file_path = outDir </> outFileName
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
    trim_mp4_video ::
         (MonadError String m, MonadIO m)
      => FilePath
      -> FilePath
      -> Int
      -> Int
      -> m ()
    trim_mp4_video in_file_path out_file_path start_seconds duration = do
      let format :: (Integral s, PrintfArg s) => s -> String
          format s =
            printf
              "%02d:%02d:%02d.000"
              (s `div` 3600)
              (s `mod` 3600 `div` 60)
              (s `div` 60)
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
        "ffmpeg"
        [ "-ss"
        , format start_seconds
        , "-t"
        , show duration
        , "-i"
        , in_file_path
        , "-async"
        , "1"
        , "-vcodec"
        , "copy"
        , "-acodec"
        , "copy"
        , out_file_path
        ]
        (Just cmd_chan)

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
          putStrLn "Creating the process..."
          (_, Just stdout_handle, Just stderr_handle, p_handle) <-
            createProcess
              (proc cmd args) {std_out = CreatePipe, std_err = CreatePipe}
          putStrLn "Waiting for process..."
          code <- waitForProcess p_handle
          putStrLn "Attaching stream handle to channel..."
          stream_handle_to_chan stdout_handle out_chan
          putStrLn "Getting error handle contents..."
          err <- hGetContents stderr_handle
          putStrLn "Returning..."
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
