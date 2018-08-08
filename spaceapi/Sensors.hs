{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{- |TODO:

- sum up wifi sensors
- delta state for if_octets
-}
module Sensors (SensorsRef, WifiLocations, newSensors, handleCollectdSensors, updateSensors, renderSensors) where

import Data.Maybe
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.Monad (when, forM_)
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import System.Time
import Data.Aeson
import qualified Data.Vector as V
import Data.Aeson.Types (emptyArray)
import Data.Scientific (fromFloatDigits)
import Debug.Trace

import qualified Collectd.Listener as C
import qualified Collectd.Packet as C

type SensorsRef = TVar Sensors

type Sensors = HM.HashMap SensorId SensorState

data SensorId = SensorId {
  _sCategory :: Text,
  _sName :: Text,
  _sLocation :: Text
} deriving (Eq, Generic)

instance Hashable SensorId


data SensorState = SensorState {
  _sValue :: Double,
  _sUnit :: Text,
  sTime :: Integer
}

type WifiLocations = HM.HashMap Text Text

sensorTimeout :: Integer
sensorTimeout = 300

newSensors :: IO SensorsRef
newSensors = newTVarIO HM.empty

updateSensor :: SensorsRef -> Integer -> Text -> Text -> Text -> Double -> Text -> STM ()
updateSensor sensorsRef now category name location value unit = do
  let k = SensorId category name location
  modifyTVar' sensorsRef $
    HM.insert k $ SensorState value unit now

updateSensors :: Text -> Value -> SensorsRef -> (Text -> Double -> IO ()) -> IO ()
updateSensors location (Object obj) sensorsRef monitorFun = do
  TOD now _ <- getClockTime

  let Array values = fromMaybe emptyArray $
                     "sensordatavalues" `HM.lookup` obj

      getValue :: Text -> Maybe Value
      getValue k = do
        (Object item) <- V.find
          (\(Object item) ->
              case "value_type" `HM.lookup` item of
                Just k' | String k == k' -> True
                _ -> False
          ) values
        "value" `HM.lookup` item

      parseDouble :: Value -> Maybe Double
      parseDouble (String t) =
        Just $ read $ T.unpack t
      parseDouble _ = Nothing

      updateSensor' = updateSensor sensorsRef now

      monitor name value = do
        let logName = T.concat [ location
                               , "-"
                               , name
                               ]
        monitorFun logName value

  -- Handle SDS011 readings
  case ( getValue "SDS_P1" >>= parseDouble
       , getValue "SDS_P2" >>= parseDouble
       ) of
    (Just pm10, Just pm2) -> do
      atomically $ do
        updateSensor' "dust" "PM2.5" location pm2 "µg/m³"
        updateSensor' "dust" "PM10" location pm10 "µg/m³"
      monitor "pm2" pm2
      monitor "pm10" pm10
    _ ->
      return ()

  -- Handle DHT22 readings
  case getValue "temperature" >>= parseDouble of
    Just temperature -> do
      atomically $
        updateSensor' "temperature" "DHT22" location temperature "°C"
      monitor "temperature" temperature
    Nothing ->
      return ()

  case getValue "humidity" >>= parseDouble of
    Just humidity -> do
      atomically $
        updateSensor' "humidity" "DHT22" location humidity "%"
      monitor "humidity" humidity
    Nothing ->
      return ()

updateSensors _ _ _ _ = return ()

interestingData :: C.Datum -> WifiLocations -> Maybe (Text, Text, Text, Double, Text)
interestingData d wifiLocations

  | (dHost `elem` allowedHosts) && dPlugin == "iwinfo" && dType == "stations" =
      case C.datumValues d of
        [C.Gauge value] ->
          let name = T.concat [dHost, " ", dPluginInstance]
          in Just ("network_connections", name, fromMaybe "" $ dHost `HM.lookup` wifiLocations, value, "stations")
        _ ->
          Nothing
  -- | (dHost == "upstream1" && dPluginInstance == "up1" || dHost == "anon1" && dPluginInstance == "ipredator") && dPlugin == "interface" && dType == "if_octets" =
  --     case C.datumValues d of
  --       [C.Derive rx, C.Derive tx] ->
  | otherwise = Nothing
  where (dHost, dPlugin, dPluginInstance, dType, _dTypeInstance) = C.datumPath d
        allowedHosts = HM.keys wifiLocations

handleCollectdSensors :: SensorsRef -> WifiLocations -> [C.Datum] -> IO ()
handleCollectdSensors sensorsRef wifiLocations datas = do
  TOD now _ <- getClockTime
  let updateSensor' = updateSensor sensorsRef now

  atomically $ forM_ datas $ \d ->
    case interestingData d wifiLocations of
      Nothing ->
        return ()
      Just (category, name, location, value, unit) ->
        trace (show d) $
        updateSensor' category name location value unit

renderSensors :: SensorsRef -> IO Value
renderSensors sensorsRef = do
  TOD now _ <- getClockTime

  (sensors, deleted) <- atomically $ do
    sensors <- readTVar sensorsRef
    let oldSize = HM.size sensors
        sensors' = HM.filter (\state ->
                                sTime state + sensorTimeout > now
                             ) sensors
        newSize = HM.size sensors
        deleted = oldSize - newSize
    when (deleted > 0) $
      writeTVar sensorsRef sensors'

    return (sensors', deleted)

  when (deleted /= 0) $
    putStrLn $ "Deleted " ++ show deleted ++ " sensor values from " ++
    show (map (\(SensorId category name _) -> (category, name)) $ HM.keys sensors)

  return $ Object $
    HM.map (Array . V.fromList) $
    HM.foldlWithKey'
    (\obj (SensorId category name location) (SensorState value unit _) ->
        let item = object [ ("name", String name)
                          , ("location", String location)
                          , ("value", Number $ fromFloatDigits value)
                          , ("unit", String unit)
                          ]
            cat = fromMaybe [] $
                  category `HM.lookup` obj
            cat' = item : cat
        in HM.insert category cat' obj
    ) HM.empty sensors
