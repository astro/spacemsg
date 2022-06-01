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
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.Time
import Data.Aeson
import qualified Data.Vector as V
import Data.Aeson.Types (emptyArray)
import Data.Scientific (fromFloatDigits)
import Data.Char (isDigit, isAlpha)
import Data.List (sortBy)
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

type WifiLocations = KM.KeyMap Text

sensorTimeout :: Integer
sensorTimeout = 300

newSensors :: IO SensorsRef
newSensors = newTVarIO HM.empty

updateSensor :: SensorsRef -> Integer -> Text -> Text -> Text -> Double -> Text -> STM ()
updateSensor sensorsRef now category name location value unit = do
  let k = SensorId category name location
  modifyTVar' sensorsRef $
    HM.insert k $ SensorState value unit now

updateSensors :: Text -> Value -> SensorsRef -> IO ()
updateSensors location (Object obj) sensorsRef = do
  TOD now _ <- getClockTime

  let Array values = fromMaybe emptyArray $
                     "sensordatavalues" `KM.lookup` obj

      getValue :: Text -> Maybe Value
      getValue k = do
        (Object item) <- V.find
          (\(Object item) ->
              case "value_type" `KM.lookup` item of
                Just k' | String k == k' -> True
                _ -> False
          ) values
        "value" `KM.lookup` item

      parseDouble :: Value -> Maybe Double
      parseDouble (String t) =
        Just $ read $ T.unpack t
      parseDouble _ = Nothing

      updateSensor' = updateSensor sensorsRef now

  -- Handle SDS011 readings
  case ( getValue "SDS_P1" >>= parseDouble
       , getValue "SDS_P2" >>= parseDouble
       ) of
    (Just pm10, Just pm2) -> do
      atomically $ do
        updateSensor' "dust" "PM2.5" location pm2 "µg/m³"
        updateSensor' "dust" "PM10" location pm10 "µg/m³"
    _ ->
      return ()

  -- Handle DHT22 readings
  case getValue "temperature" >>= parseDouble of
    Just temperature -> do
      atomically $
        updateSensor' "temperature" "DHT22" location temperature "°C"
    Nothing ->
      return ()

  case getValue "humidity" >>= parseDouble of
    Just humidity -> do
      atomically $
        updateSensor' "humidity" "DHT22" location humidity "%"
    Nothing ->
      return ()

updateSensors _ _ _ = return ()

interestingData :: C.Datum -> WifiLocations -> Maybe (Text, Text, Text, Double, Text)
interestingData d wifiLocations

  | (K.fromText dHost `elem` allowedHosts) && dPlugin == "iwinfo" && dType == "stations" =
      case C.datumValues d of
        [C.Gauge value] ->
          let name = T.concat [dHost, " ", dPluginInstance]
          in Just ("network_connections", name, fromMaybe "" $ K.fromText dHost `KM.lookup` wifiLocations, value, "stations")
        _ ->
          Nothing
  -- | (dHost == "upstream1" && dPluginInstance == "up1" || dHost == "anon1" && dPluginInstance == "ipredator") && dPlugin == "interface" && dType == "if_octets" =
  --     case C.datumValues d of
  --       [C.Derive rx, C.Derive tx] ->
  | otherwise = Nothing
  where (dHost, dPlugin, dPluginInstance, dType, _dTypeInstance) = C.datumPath d
        allowedHosts = KM.keys wifiLocations

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

renderSensors :: SensorsRef -> IO (Maybe Value)
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

  return (if HM.null sensors
    then Nothing
    else Just $ Object $
      KM.fromHashMap $
      HM.map (Array . V.fromList) $
      HM.foldlWithKey
      (\obj (SensorId category name location) (SensorState value unit _) ->
          let item = object [ ("name", String name)
                            , ("location", String location)
                            , ("value", Number $ fromFloatDigits value)
                            , ("unit", String unit)
                            ]
              cat = fromMaybe [] $
                    K.fromText category `HM.lookup` obj
              cat' = sortBy cmpItems $
                     item : cat
          in HM.insert (K.fromText category) cat' obj
      ) HM.empty sensors
    )

  where cmpItems (Object item1) (Object item2) =
          let tokenizeField field item =
                case field `KM.lookup` item of
                  Just (String s) -> tokenize s
                  _ -> []
              compareFields field =
                tokenizeField field item1 `compare` tokenizeField field item2
              orderOr f f' =
                case f of
                  EQ -> f'
                  ordering -> ordering
          in compareFields "name" `orderOr`
             compareFields "location" `orderOr`
             compareFields "unit" `orderOr`
             compareFields "value"
        cmpItems _value1 _value2 =
          trace "Comparing non-items!"
          EQ

data Token = NumberToken Integer
           | TextToken Text
           deriving (Show, Eq, Ord)

tokenize :: Text -> [Token]
tokenize input
  | T.null input = []
  | isDigit $ T.head input =
      let (number, input') = T.span isDigit input
          n = read $ T.unpack number
      in NumberToken n : tokenize input'
  | isAlpha $ T.head input =
      let (t, input') = T.span isAlpha input
      in TextToken t : tokenize input'
  | otherwise =
      tokenize $ T.tail input
