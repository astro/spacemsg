{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Sensors (SensorsRef, newSensors, updateSensors, renderSensors) where

import Data.Maybe
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.Monad (when)
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import System.Time
import Data.Aeson
import qualified Data.Vector as V
import Data.Aeson.Types (emptyArray)
import Data.Scientific (fromFloatDigits)

type SensorsRef = TVar Sensors

type Sensors = HM.HashMap SensorId SensorState

data SensorId = SensorId {
  _sCategory :: Text,
  _sName :: Text,
  _sLocation :: Text
} deriving (Eq, Generic)

instance Hashable SensorId


data SensorState = SensorState {
  _sValue :: Float,
  _sUnit :: Text,
  sTime :: Integer
}

sensorTimeout :: Integer
sensorTimeout = 300

newSensors :: IO SensorsRef
newSensors = newTVarIO HM.empty

updateSensors :: Text -> Value -> SensorsRef -> IO ()
updateSensors location (Object obj) sensorsRef = do
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

      parseFloat :: Value -> Maybe Float
      parseFloat (String t) =
        Just $ read $ T.unpack t
      parseFloat _ = Nothing

      updateSensor category name value unit = do
        let k = SensorId category name location
        modifyTVar' sensorsRef $
          HM.insert k $ SensorState value unit now

  -- Handle SDS011 readings
  case ( getValue "SDS_P1" >>= parseFloat
       , getValue "SDS_P2" >>= parseFloat
       ) of
    (Just pm10, Just pm2) ->
      atomically $ do
      updateSensor "dust" "PM2.5" pm2 "µg/m³"
      updateSensor "dust" "PM10" pm10 "µg/m³"
    _ ->
      return ()

updateSensors _ _ _ = return ()

renderSensors :: SensorsRef -> IO Value
renderSensors sensorsRef = do
  TOD now _ <- getClockTime

  sensors <- atomically $ do
    sensors <- readTVar sensorsRef
    let oldSize = HM.size sensors
        sensors' = HM.filter (\state ->
                                sTime state + sensorTimeout > now
                             ) sensors
        newSize = HM.size sensors
    when (oldSize /= newSize) $
      writeTVar sensorsRef sensors'

    return sensors'

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
