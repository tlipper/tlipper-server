{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module AWS.API where

import AWS.Signing (presign)
import qualified Conduit
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.Chan.Class
import Control.Concurrent.Chan.WriteOnly
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.AWS as AWS
import Data.Functor (void)
import Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.S3 as AWS
import qualified Network.AWS.S3.StreamingUpload as AWSSU
import Network.HTTP.Client (ManagerSettings(..), Request(..), httpLbs)
import Network.HTTP.Client.TLS
import System.IO
import System.Posix.Files (fileSize, getFileStatus)
import qualified Text.Printf as T

chunkSize = 20 * 10 ^ 6 -- Minimum chunk size in S3 multipart uploads is 5MB.

pr :: Conduit.MonadIO m => Conduit.ConduitT i i m ()
pr = do
  Conduit.await >>= \case
    Just v -> do
      liftIO $ putStrLn "CHUNK!"
      Conduit.yield v
    Nothing -> pure ()

data UploadProgress
  = UploadFinished T.Text
  -- ^ Text represents the download url.
  | UploadInProgress Int
  -- ^ Int represents the completion percentage.
  deriving (Eq, Show)

putChunkedFile ::
     AWS.Credentials
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> Maybe AWS.ChunkSize
  -> FilePath
  -> WriteOnlyChan UploadProgress
  -> IO ()
putChunkedFile credentials b@(AWS.BucketName bucket_name) k@(AWS.ObjectKey object_key) mbChunkSize f updateChan = do
  lgr <- AWS.newLogger AWS.Debug stdout
  size <- fileSize <$> getFileStatus f
  putStrLn "SIZE:"
  print size
  let chunk_size = AWSSU.minimumChunkSize
  env <-
    AWS.newEnv credentials <&> set AWS.envRegion AWS.Ireland .
    set AWS.envLogger lgr
  void $ async $ AWS.runResourceT . AWS.runAWST env $ do
    let got_update partnum bufsize = do
          let completion :: Double =
                fromIntegral partnum * fromIntegral chunk_size /
                fromIntegral size
          let percentage :: Int = (floor (100 * completion)) :: Int
          liftIO $ writeChan updateChan $ UploadInProgress percentage
          liftIO $ putStrLn $ T.printf "Completed %d" percentage
          pure ()
    resp <-
      Conduit.runConduit $ Conduit.sourceFile f Conduit..|
      AWSSU.streamUpload
        (Just chunk_size)
        got_update
        (AWS.createMultipartUpload b k)
    let url =
          "https://" <> bucket_name <> ".s3-eu-west-1.amazonaws.com/" <>
          object_key
    liftIO $ writeChan updateChan $ UploadFinished url
    pure ()
