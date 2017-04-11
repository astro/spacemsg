{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module EKG (Monitor, startMonitor, incCounter, setGauge) where

import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import System.Remote.Counter
import System.Remote.Gauge hiding (inc)
import System.Remote.Monitoring
import Data.IORef
import GHC.Generics (Generic)
import Data.Hashable (Hashable)


data MetricKey = KeyGauge Text
               | KeyCounter Text
               deriving (Show, Eq, Generic)
instance Hashable MetricKey

data MetricValue = ValueGauge Gauge
                 | ValueCounter Counter

data Monitor = Monitor {
  monitorMetrics :: IORef (HM.HashMap MetricKey MetricValue),
  monitorEKG :: Server
}

startMonitor :: Int -> IO Monitor
startMonitor port =
  Monitor <$>
  newIORef HM.empty <*>
  forkServer "::" port

getMetric :: MetricKey -> Monitor -> IO MetricValue
getMetric key monitor = do
  metrics <- readIORef $ monitorMetrics monitor
  case HM.lookup key metrics of
    Just value -> return value
    Nothing -> do
      let server = monitorEKG monitor
      new <- case key of
               KeyGauge name ->
                 ValueGauge <$>
                 getGauge name server
               KeyCounter name ->
                 ValueCounter <$>
                 getCounter name server
      writeIORef (monitorMetrics monitor) $
        HM.insert key new metrics
      return new

incCounter :: Text -> Monitor -> IO ()
incCounter name monitor = do
  ValueCounter counter <- getMetric (KeyCounter name) monitor
  inc counter

setGauge :: RealFrac v => Text -> v -> Monitor -> IO ()
setGauge name value monitor = do
  ValueGauge gauge <- getMetric (KeyGauge name) monitor
  set gauge $ truncate $ 1000 * value
