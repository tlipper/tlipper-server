{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module AWS.API where

import AWS.Signing (presign)
import qualified Conduit
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

putChunkedFile ::
     AWS.Credentials
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> Maybe AWS.ChunkSize
  -> FilePath
  -> IO ()
putChunkedFile credentials b k mbChunkSize f = do
  lgr <- AWS.newLogger AWS.Debug stdout
  size <- fileSize <$> getFileStatus f
  putStrLn "SIZE:"
  print size
  let chunk_size = AWSSU.minimumChunkSize
  env <-
    AWS.newEnv credentials <&> set AWS.envRegion AWS.Ireland .
    set AWS.envLogger lgr
  AWS.runResourceT . AWS.runAWST env $ do
    let got_update partnum bufsize = do
          let completion :: Double = fromIntegral partnum * fromIntegral chunk_size / fromIntegral size
          liftIO $ putStrLn $ T.printf "Completed %d" ((floor (100 * completion)) :: Int)
          pure ()
    resp <-
      Conduit.runConduit $ Conduit.sourceFile f Conduit..|
      AWSSU.streamUpload
        (Just chunk_size)
        got_update
        (AWS.createMultipartUpload b k)
    pure ()
