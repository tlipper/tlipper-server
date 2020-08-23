{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AWS.Signing
  ( presign
  ) where

import Control.Lens ((%~), (&), view)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString hiding (filter)
import Data.Monoid (appEndo, getDual)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Network.AWS
  ( AWSRequest
  , Auth
  , ClientRequest
  , HasEnv
  , MonadAWS
  , Region
  , Seconds(Seconds)
  , Service
  , environment
  , liftAWS
  , request
  , rqHeaders
  , rqPresign
  , rqService
  , sgRequest
  , withAuth
  )
import Network.AWS.Env (Env(Env), _envAuth, _envOverride, _envRegion)
import Network.AWS.Request (requestURL)
import Network.AWS.S3 (putObject)
import Network.HTTP.Types.Header (Header)

-- |Re-implements 'Network.AWS.presignURL' with the errant header removed.
presignURL ::
     (MonadAWS m, AWSRequest a)
  => UTCTime -- ^ Signing time.
  -> Seconds -- ^ Expiry time.
  -> a -- ^ Request to presign.
  -> m ByteString
presignURL ts ex = liftAWS . presignURL_ ts ex

-- |Re-implements 'Control.Monad.Trans.AWS.presignURL' with the errant header removed.
presignURL_ ::
     (MonadIO m, MonadReader r m, HasEnv r, AWSRequest a)
  => UTCTime -- ^ Signing time.
  -> Seconds -- ^ Expiry time.
  -> a -- ^ Request to presign.
  -> m ByteString
presignURL_ ts ex = liftM requestURL . presign ts ex

-- |Re-implements 'Control.Monad.Trans.AWS.presign' with the errant header removed.
presign ::
     (MonadIO m, MonadReader r m, HasEnv r, AWSRequest a)
  => UTCTime -- ^ Signing time.
  -> Seconds -- ^ Expiry time.
  -> a -- ^ Request to presign.
  -> m ClientRequest
presign ts ex x = do
  Env {..} <- view environment
  presignWithHeaders
    (appEndo (getDual _envOverride))
    noExpect
    _envAuth
    _envRegion
    ts
    ex
    x
  where
    noExpect = filter $ ((/= "Expect") . fst)

-- |See 'Network.AWS.Presign.presignWith'.
presignWithHeaders ::
     (MonadIO m, AWSRequest a)
  => (Service -> Service)
  -> ([Header] -> [Header])
  -> Auth
  -> Region
  -> UTCTime -- ^ Signing time.
  -> Seconds -- ^ Expiry time.
  -> a -- ^ Request to presign.
  -> m ClientRequest
presignWithHeaders fs fh a r ts ex x =
  withAuth a $ \ae ->
    return $!
    sgRequest $
    rqPresign ex (request x & rqService %~ fs & rqHeaders %~ fh) ae r ts
