{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe
import Control.Monad (void)
import Yesod hiding (warp)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types.Status (status204, status302, status404)
import Data.Text (Text)
import Control.Concurrent (forkIO)
import qualified Network.Wai.Handler.Warp

import qualified HQSwitch as Sw
import Sensors
import Collectd.Listener (runListener)
import EKG

data App = App {
      appMonitor :: Monitor,
      appJSON :: Object,
      appSwitch :: IO Sw.Status,
      appSensors :: SensorsRef
    }

mkYesod "App" [parseRoutes|
/spaceapi.json SpaceApiR GET
/status.png StatusIconR GET
/sensors/#Text SensorsEndpointR POST
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing

withMonitor_ :: Handler ((Monitor -> IO a) -> IO a)
withMonitor_ = do
  monitor <- appMonitor <$> getYesod
  return $ \f -> f monitor

withMonitor :: (Monitor -> IO a) -> Handler a
withMonitor f =
  (appMonitor <$> getYesod) >>= liftIO . f

getSpaceApiR :: Handler RepJson
getSpaceApiR = do
  withMonitor $ incCounter "getSpaceApi"

  addHeader "Access-Control-Allow-Origin" "*"

  App { appJSON = obj, appSwitch = sw, appSensors = sensorsRef } <- getYesod
  swSt <- liftIO sw
  sensorsObj <- liftIO $ renderSensors sensorsRef
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
      obj' = HM.insert "state" stateObj' $
             HM.insert "open" (toJSON open) $
             HM.insert "status" (toJSON message) $
             maybe id (HM.insert "sensors") sensorsObj $
             obj
  return $ RepJson $ toContent $ Object obj'

getStatusIconR :: Handler ()
getStatusIconR = do
  withMonitor $ incCounter "getStatusIcon"

  App { appJSON = obj, appSwitch = sw } <- getYesod
  swSt <- liftIO sw
  let state = Sw.stState swSt
      stateKey = fromMaybe "error" $
                 lookup state
                 [(Sw.On, "open"),
                  (Sw.Full, "full"),
                  (Sw.Off, "closed")
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

postSensorsEndpointR :: Text -> Handler ()
postSensorsEndpointR location = do
  withMonitor $ incCounter "postSensorsEndpoint"
  parsedBody <- parseJsonBody
  case parsedBody of
    (Success obj :: Result Value) -> do
      state <- appSensors <$> getYesod
      monitorFun <- withMonitor_
      let monitorFun' name value = monitorFun $ setGauge name value
      liftIO $ updateSensors location obj state monitorFun'
      sendResponseStatus status204 ()
    _ ->
      sendResponseStatus status204 ()

-- TODO: toWaiApp(Plain) + CORS middleware
main :: IO ()
main = do
  ekg <- startMonitor 3001
  sensorsRef <- newSensors
  let loadJson :: FromJSON a => String -> IO a
      loadJson filename =
        fromMaybe (error $ "Cannot decode " ++ filename) <$>
        decode <$>
        LBC.readFile filename

  wifiLocations <- loadJson "wifi-locations.json"
  putStrLn $ "Wifi locations: " ++ show wifiLocations
  void $ forkIO $
    runListener "::" "25826" $ handleCollectdSensors sensorsRef wifiLocations

  spaceapiJson <- loadJson "spaceapi.json"
  app <- App ekg spaceapiJson <$>
         Sw.start <*>
         pure sensorsRef
  warp 3000 app

warp :: YesodDispatch site => Int -> site -> IO ()
warp port site =
  let settings = Network.Wai.Handler.Warp.setPort port $
                 Network.Wai.Handler.Warp.setHost "*6" $
                 Network.Wai.Handler.Warp.setServerName "SpaceAPI" $
                 Network.Wai.Handler.Warp.defaultSettings
  in toWaiApp site >>= Network.Wai.Handler.Warp.runSettings settings
