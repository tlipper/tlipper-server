{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Monitoring where

import Control.Concurrent.Async.Lifted (Async, async, wait, waitEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Functor (void)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import Network.HTTP.Types
import Network.Wai.Middleware.Prometheus (applicationMetrics)
import qualified Network.Wai.Middleware.Prometheus as P
import Server.API (endpoints)
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as P
import qualified System.Metrics.Prometheus.Http.Scrape as P
import qualified System.Metrics.Prometheus.Metric.Counter as P
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.Metric.Histogram as P
import qualified System.Metrics.Prometheus.MetricId as P

unknownPathString :: T.Text
unknownPathString = "unknown path"

unknownMethodString :: BS.ByteString
unknownMethodString = "unknown method"

data AppMetrics =
  AppMetrics
    {
    }

data HttpMetrics =
  HttpMetrics
    { responseCounter :: P.Counter
    , responseTimesHistogram :: P.Histogram
    }

newtype ServantMetrics =
  ServantMetrics (H.HashMap (T.Text, Method) HttpMetrics)

registerAppMetrics :: MonadIO m => P.RegistryT m AppMetrics
registerAppMetrics = pure AppMetrics {}

registerServantMetrics :: MonadIO m => P.RegistryT m ServantMetrics
registerServantMetrics = do
  fmap (ServantMetrics . H.fromList) $
    for (([unknownPathString], unknownMethodString) : endpoints) $ \(pathSegments, method) -> do
      let path = T.intercalate "/" pathSegments
      let labels =
            P.fromList $ [("path", path), ("method", T.decodeUtf8 method)]
      responseCounter <- P.registerCounter "responses" labels
      let exportDurationBounds =
            [1 .. 20] <>
            [30,40 .. 200] <>
            [300,400 .. 900] <> [1000,2000 .. 9000] <> [10000,15000 .. 180000]
      responseTimesHistogram <-
        P.registerHistogram
          "export_duration_milliseconds"
          labels
          exportDurationBounds
      pure $ ((path, method), HttpMetrics {..})

withPrometheus :: (ServantMetrics -> AppMetrics -> IO ()) -> IO ()
withPrometheus doIO = do
  P.runRegistryT $ do
    appMetrics <- registerAppMetrics
    servantMetrics <- registerServantMetrics
    ioThread <- liftIO $ async $ doIO servantMetrics appMetrics
    P.serveMetricsT 8081 ["metrics"]
