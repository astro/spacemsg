{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Maybe
import Control.Applicative
import Yesod
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types.Status (status302, status404)

import qualified HQSwitch as Sw
import qualified Sensors as Sensors

-- TODO: .cabal
data App = App {
      appJSON :: Object,
      appSwitch :: IO Sw.Status,
      appSensors :: IO Sensors.Status
    }

mkYesod "App" [parseRoutes|
/spaceapi.json SpaceApiR GET
/status.png StatusIconR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing

getSpaceApiR :: Handler RepJson
getSpaceApiR = do
  addHeader "Access-Control-Allow-Origin" "*"
  
  App { appJSON = obj, appSwitch = sw, appSensors = se } <- getYesod
  swSt <- liftIO sw
  seSt <- liftIO se
  let stateObj =
          fromMaybe [] $
          HM.lookup "state" obj >>=
          parseMaybe parseJSON
      open = Sw.isOpen swSt
      message = Sw.stMessage swSt
      stateObj' =
          object $
          [ "open" .= open,
            "message" .= message,
            "lastchange" .= Sw.stLastChange swSt
          ] ++ stateObj
      mkSensor :: Text -> Text -> Text -> Text -> Maybe Value
      mkSensor key name unit description =
          let sensorObj value =
                object [ "name" .= name
                       , "value" .= value
                       , "unit" .= unit
                       , "description" .= description
                       ]
              
          in Sensors.stState seSt >>=
             HM.lookup key >>=
             return . sensorObj
      sensorsObj =
          object $
          [ "network_connections" .=
            catMaybes
            [ mkSensor "ratbert.hq.c3d2.de:wlan0-stations"
              "ratbert WiFi 2.4 GHz" "stations" "next to the DSL splitter"
            , mkSensor "dogbert.hq.c3d2.de:wlan0-stations"
              "dogbert WiFi 2.4 GHz" "stations" "library"
            , mkSensor "dogbert.hq.c3d2.de:wlan1-stations"
              "dogbert WiFi 5 GHz" "stations" "library"
            , mkSensor "dogbert.hq.c3d2.de:wlan1-1-stations"
              "dogbert WiFi 5 GHz extra" "stations" "library"
            ]
          ]
      obj' = HM.insert "state" stateObj' $
             HM.insert "open" (toJSON open) $
             HM.insert "status" (toJSON message) $
             HM.insert "sensors" sensorsObj $
             obj
  return $ RepJson $ toContent $ Object obj'

getStatusIconR :: Handler ()
getStatusIconR = do
  App { appJSON = obj, appSwitch = sw } <- getYesod
  swSt <- liftIO sw
  let state = Sw.stState swSt
      stateKey = fromMaybe "closed" $
                 lookup state
                 [(Sw.On, "open"),
                  (Sw.Full, "full")
                 ]
      findUrl = (obj .: "state") >>=
                (.: "icon") >>=
                (.: stateKey) >>=
                parseJSON
      mUrl :: Maybe String
      mUrl = parseMaybe (const findUrl) $ Object obj
  case mUrl of
    Just url ->
        redirectWith status302 url
    Nothing ->
        sendResponseStatus status404 ()

-- TODO: toWaiApp(Plain) + CORS middleware
main :: IO ()
main = do
  app <- App <$>
         fromMaybe (error "Cannot load spaceapi.json") <$>
         decode <$>
         LBC.readFile "spaceapi.json" <*>
         Sw.start <*>
         Sensors.start
  warp 3000 app
