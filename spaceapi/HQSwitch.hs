{-# LANGUAGE OverloadedStrings #-}
module HQSwitch where

import Control.Monad
import Control.Concurrent
import Control.Exception (catch, SomeException)
import System.Time
import Control.Concurrent.STM
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson hiding (Error)
import qualified Data.HashMap.Strict as HM


data State = Off
           | On
           | Full
           | Error
             deriving (Eq, Show)

data Status = Status {
      stStatus :: Maybe Int,
      stLocked :: Maybe Bool,
      stLastChange :: Integer
    } deriving (Show)

stState :: Status -> State
stState Status { stStatus = Nothing } =
    Error
stState Status { stStatus = Just value } =
    case value of
      0 -> Off
      1 -> On
      2 -> Full
      _ -> Error

isOpen :: Status -> Bool
isOpen status =
    case stState status of
      On -> True
      Full -> True
      _ -> False

stMessage :: Status -> String
stMessage status =
  let message =
        case stState status of
          Off -> "GCHQ is off"
          On -> "GCHQ is on"
          Full -> "Party mode"
          _ -> "Error"
      lockSuffix =
        case stLocked status of
          Just False -> " (unlocked)"
          Just True -> " (locked)"
          Nothing -> ""
  in message ++ lockSuffix

poll :: IO (Maybe Int, Maybe Bool)
poll = do
     manager <- newManager defaultManagerSettings
     req <- parseUrlThrow "http://schalter.hq.c3d2.de/schalter.json"
     res <- httpLbs req manager
     let schalter =
           case statusCode (responseStatus res) of
             200 ->
               let mObj :: Maybe (HM.HashMap String Int)
                   mObj = decode $ responseBody res
               in mObj >>= HM.lookup "status"
             _ ->
               Nothing

     req' <- parseUrlThrow "http://schalter.hq.c3d2.de/door.json"
     res' <- httpLbs req' manager
     let door =
           case statusCode (responseStatus res') of
             200 ->
               let mObj :: Maybe (HM.HashMap String Bool)
                   mObj = decode $ responseBody res'
               in mObj >>= HM.lookup "locked"
             _ ->
               Nothing

     return (schalter, door)

run :: TVar Status -> IO ()
run tStatus = forever $ do
    (status, door) <- poll `catch` \e -> do
      print (e :: SomeException)
      return (Nothing, Nothing)

    TOD now _ <- getClockTime
    changed <-
      atomically $ do
                oldStatus <- readTVar tStatus
                let newStatus = Status {
                                  stStatus = status,
                                  stLocked = door,
                                  stLastChange = now
                                }
                case stState oldStatus /= stState newStatus of
                  True -> do
                     writeTVar tStatus newStatus
                     return $ Just $ stState newStatus
                  _ -> return Nothing
    case changed of
      Just state ->
        putStrLn $ show now ++ " " ++ show state
      Nothing ->
        -- | 5 000 000 microseconds
        threadDelay 5000000

start :: IO (IO Status)
start = do
  TOD now _ <- getClockTime
  tStatus <- newTVarIO $ Status Nothing Nothing now
  _thr <- forkIO $ run tStatus
  return $ readTVarIO tStatus
