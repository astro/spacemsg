module HQSwitch where

import Control.Monad
import Data.Monoid
import Data.String
import System.ZMQ3.Monadic hiding (Off, On)
import Control.Concurrent
import qualified Data.ByteString as B
import System.Time
import Control.Concurrent.STM


data State = Off
           | On
           | Full
           | Error
             deriving (Eq, Show)

data Status = Status {
      stState :: State,
      stLastChange :: Integer
    } deriving (Show)

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
      req <- socket Req
      connect req "tcp://beere.hq.c3d2.de:5555"
      forever $ do
        send req [] mempty
        reply <- receive req

        liftIO $ do
          TOD now _ <- getClockTime
          let state =
                  case B.unpack reply of
                    [0] -> Off
                    [1] -> On
                    [2] -> Full
                    _ -> Error
          changed <- atomically $ do
            oldStatus <- readTVar tStatus
            case stState oldStatus == state of
              True ->
                return False
              False ->
                do writeTVar tStatus $ Status state now
                   return True
          when (changed) $
            putStrLn $ show now ++ " " ++ show state
          threadDelay 1000000

start :: IO (IO Status)
start = do
  TOD now _ <- getClockTime
  tStatus <- newTVarIO $ Status Error now
  _thr <- forkIO $ run tStatus
  return $ readTVarIO tStatus
