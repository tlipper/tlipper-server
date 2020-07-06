{-# LANGUAGE OverloadedStrings #-}

module AWS.API where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.AWS as AWS
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.S3 as AWS
import System.IO

-- https://github.com/brendanhay/amazonka/issues/537 is blocking us from using the amazonka library. For now we use aws cli.
putChunkedFile ::
     AWS.Credentials
  -> AWS.Region
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> Maybe AWS.ChunkSize
  -> FilePath
  -> IO ()
putChunkedFile credentials r b k mbChunkSize f = do
  let chunkSize = fromMaybe AWS.defaultChunkSize mbChunkSize
  lgr <- AWS.newLogger AWS.Debug stdout
  env <- AWS.newEnv credentials <&> set AWS.envLogger lgr . set AWS.envRegion r
  AWS.runResourceT . AWS.runAWST env $ do
    bdy <- AWS.chunkedFile (8 * 1024) f
    void . AWS.send $ set AWS.poContentLength Nothing $
      set AWS.poContentEncoding (Just "aws-chunked") $
      AWS.putObject b k bdy
    liftIO $ T.putStrLn $ "Successfully Uploaded: " <> AWS.toText f <> " to " <>
      AWS.toText b <>
      " - " <>
      AWS.toText k
