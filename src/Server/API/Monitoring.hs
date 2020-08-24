{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.API.Monitoring where

import Control.Exception (bracket)
import Data.Functor (void)
import qualified Data.HashMap.Strict as H
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Monitoring
import Network.HTTP.Types
import Network.Wai (Middleware)
import Server.API.Endpoints
import qualified System.Metrics.Prometheus.Metric.Counter as P
import qualified System.Metrics.Prometheus.Metric.Histogram as P

responseCount :: P.Counter -> Middleware
responseCount counter application request respond = do
  application request respond <* P.inc counter

responseTimeDistribution :: P.Histogram -> Middleware
responseTimeDistribution histogram application request respond = do
  bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
      t2 <- getCurrentTime
      let dt = diffUTCTime t2 t1
          t = fromRational $ (* 1000) $ toRational dt
      P.observe t histogram

monitorServant :: HasEndpoints api => Proxy api -> ServantMetrics -> Middleware
monitorServant proxy (ServantMetrics ms) application =
  \request respond -> do
    let (path, method) =
          case getEndpoint proxy request of
            Nothing -> (unknownPathString, unknownMethodString)
            Just (ps, method) -> (T.intercalate "/" ps, method)
    let HttpMetrics {..} = ms H.! (path, method)
        application' =
          responseCount responseCounter .
          responseTimeDistribution responseTimesHistogram $
          application
    application' request respond
