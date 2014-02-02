{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Maybe
import Control.Applicative
import Yesod
import Data.Aeson
import Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as LBC

import qualified HQSwitch as Sw

-- TODO: disable cookies, .cabal
data App = App {
      appJSON :: Object,
      appSwitch :: IO Sw.Status
    }

mkYesod "App" [parseRoutes|
/spaceapi.json SpaceApiR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing

getSpaceApiR :: Handler RepJson
getSpaceApiR = do
  App { appJSON = obj, appSwitch = sw } <- getYesod
  swSt <- liftIO sw
  let fromObj obj = case fromJSON obj of
                      Success a -> Just a
                      Error _ -> Nothing
      stateObj = object $ [
                  "open" .= Sw.isOn swSt,
                  "message" .= Sw.stMessage swSt,
                  "lastchange" .= Sw.stLastChange swSt
                 ] ++ fromMaybe [] (HM.lookup "state" obj >>= fromObj)
      obj' = HM.insert "state" stateObj obj
  return $ RepJson $ toContent $ Object obj'

main :: IO ()
main = do
  app <- App <$>
         fromMaybe (error "Cannot load spaceapi.json") <$>
         decode <$>
         LBC.readFile "spaceapi.json" <*>
         Sw.start
  warp 3000 app
