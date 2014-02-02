{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Yesod
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBC


data App = App {
      appJSON :: Object
    }

mkYesod "App" [parseRoutes|
/spaceapi.json SpaceApiR GET
|]

instance Yesod App where

getSpaceApiR :: Handler RepJson
getSpaceApiR = do
  App { appJSON = json } <- getYesod
  return $ RepJson $ toContent $ Object json


main = do
  Just json <- decode <$> LBC.readFile "spaceapi.json"
  warp 3000 $ App { appJSON = json }
