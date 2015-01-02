{-# LANGUAGE OverloadedStrings #-}
module HQSwitch where

import Control.Monad
import Data.Monoid
import System.ZMQ4.Monadic hiding (Off, On)
import Control.Concurrent
import qualified Data.ByteString.Char8 as BC
import System.Time
import Control.Concurrent.STM
import Data.Maybe


data State = Off
           | On
           | Full
           | Error
             deriving (Eq, Show)

data Status = Status {
      stPins :: Maybe (Bool, Bool),
      stLastChange :: Integer
    } deriving (Show)

stState :: Status -> State
stState Status { stPins = Nothing } =
    Error
stState Status { stPins = Just pins } =
    case pins of
      (True, _) -> On
      (False, True) -> Full
      (False, False) -> Off


isOpen :: Status -> Bool
isOpen status =
    case stState status of
      On -> True
      Full -> True
      _ -> False

stMessage :: Status -> String
stMessage status =
    case stState status of
      Off -> "GCHQ is off"
      On -> "GCHQ is on"
      Full -> "GCHQ is full"
      _ -> "Error"

run :: TVar Status -> IO ()
run tStatus =
    runZMQ $ do
      sub <- socket Sub
      connect sub "tcp://beere.hq.c3d2.de:12345"
      subscribe sub "23"
      subscribe sub "24"
      forever $ do
        reply <- receive sub

        liftIO $ do
          print ("received", reply)
          TOD now _ <- getClockTime
          let updatePins (a, b) =
                  case BC.take 6 reply of
                    "23:0: " -> (False, b)
                    "23:1: " -> (True, b)
                    "24:0: " -> (a, False)
                    "24:1: " -> (a, True)
                    _ -> (a, b)
          changed <- atomically $ do
            oldStatus <- readTVar tStatus
            let newStatus = Status {
                                    stPins =
                                        Just $
                                        updatePins $
                                        fromMaybe (False, False) $
                                        stPins oldStatus,
                                    stLastChange = now
                                  }
            writeTVar tStatus newStatus
            return $ if stState oldStatus /= stState newStatus
                     then Just $ stState newStatus
                     else Nothing
          case changed of
            Just state ->
                       putStrLn $ show now ++ " " ++ show state
            Nothing ->
                       threadDelay 1000

start :: IO (IO Status)
start = do
  TOD now _ <- getClockTime
  tStatus <- newTVarIO $ Status Nothing now
  _thr <- forkIO $ run tStatus
  return $ readTVarIO tStatus
