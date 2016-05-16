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

-- TODO: .cabal
data App = App {
      appJSON :: Object,
      appSwitch :: IO Sw.Status
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
  
  App { appJSON = obj, appSwitch = sw } <- getYesod
  swSt <- liftIO sw
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

-- TODO: toWaiApp(Plain) + CORS middleware
main :: IO ()
main = do
  app <- App <$>
         fromMaybe (error "Cannot load spaceapi.json") <$>
         decode <$>
         LBC.readFile "spaceapi.json" <*>
         Sw.start
  warp 3000 app
