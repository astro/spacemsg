{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe
import Control.Monad (void)
import Yesod
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types.Status (status204, status302, status404)
import Data.Text (Text)
import Control.Concurrent (forkIO)

import qualified HQSwitch as Sw
import Sensors
import Collectd.Listener (runListener)

-- TODO: .cabal
data App = App {
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

getSpaceApiR :: Handler RepJson
getSpaceApiR = do
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
             HM.insert "sensors" sensorsObj $
             obj
  return $ RepJson $ toContent $ Object obj'

getStatusIconR :: Handler ()
getStatusIconR = do
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
postSensorsEndpointR name = do
  (Success obj :: Result Value) <- parseJsonBody
  state <- appSensors <$> getYesod
  liftIO $ updateSensors name obj state
  sendResponseStatus status204 ()

-- TODO: toWaiApp(Plain) + CORS middleware
main :: IO ()
main = do
  sensorsRef <- newSensors
  void $ forkIO $
    runListener "::" "25826" $ handleCollectdSensors sensorsRef
  app <- App <$>
         fromMaybe (error "Cannot load spaceapi.json") <$>
         decode <$>
         LBC.readFile "spaceapi.json" <*>
         Sw.start <*>
         pure sensorsRef
  warp 3001 app
