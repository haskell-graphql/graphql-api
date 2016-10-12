-- | Prometheus instrumentation for graphql-haskell.
module GraphQL.Instrument
  ( metrics
  , requestDuration
  , instrumentApp
  ) where

import Protolude

import Data.ByteString.Builder (byteString)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom

-- | Core information about HTTP requests:
--
-- Labels:
-- * handler: the name of the application
-- * method: the HTTP method requested
-- * status_code: the HTTP response code
--
-- Actual metric is the latency of the request.
type RequestDuration = Prom.Metric (Prom.Vector Prom.Label3 Prom.Summary)

requestDuration :: IO RequestDuration
requestDuration =
  Prom.vector ("handler", "method", "status_code") $
  Prom.summary info Prom.defaultQuantiles
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in microseconds."

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp
  :: RequestDuration -- ^ The metric to instrument
  -> Text -- ^ The label used to identify this app
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentApp metric handler app req respond = do
  start <- getCurrentTime
  app
    req
    (\res -> do
       recordResult start (HTTP.statusCode (Wai.responseStatus res))
       respond res) `onException`
    recordResult start (500 :: Integer)
  where
    recordResult start statusCode = do
      end <- getCurrentTime
      let latency = fromRational $ toRational (end `diffUTCTime` start)
      Prom.withLabel (toS handler, method, status) (Prom.observe latency) metric
      where
        method = toS (Wai.requestMethod req)
        status = show statusCode

-- | Application that serves the Prometheus /metrics page regardless of what
-- was requested.
metrics :: Wai.Application
metrics = const respondWithMetrics

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
  content <- Prom.exportMetricsAsText
  respond $ Wai.responseBuilder HTTP.status200 headers $ byteString content
  where
    headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
