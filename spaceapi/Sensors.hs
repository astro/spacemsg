module Sensors where

import Control.Monad
import Data.Monoid
import Data.String
import System.ZMQ3.Monadic hiding (Off, On)
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.Time
import Control.Concurrent.STM
import qualified Data.Aeson as JSON


data Status = Status {
      stState :: Maybe JSON.Value,
      stLastUpdate :: Integer,
      stInterval :: Int
    } deriving (Show)

run :: TVar Status -> IO ()
run tStatus =
    runZMQ $ do
      req <- socket Req
      connect req "tcp://localhost:5555"
      forever $ do
        liftIO $ putStrLn "send to sensors"
        send req [] mempty
        liftIO $ putStrLn "sent to sensors"
        reply <- receive req
        liftIO $ putStrLn $ "reply from sensors" ++ show reply

        let mValue = JSON.decode $ LB.fromChunks [reply]

        liftIO $ do
          TOD now _ <- getClockTime
          print (now, mValue)
          interval <- atomically $ do
            oldStatus <- readTVar tStatus
            let newStatus =
                  oldStatus { stState = mValue,
                              stLastUpdate = now
                            }
            writeTVar tStatus newStatus
            return $ stInterval newStatus
          threadDelay $ interval * 1000000

start :: IO (IO Status)
start = do
  TOD now _ <- getClockTime
  tStatus <- newTVarIO $ Status Nothing now 10
  _thr <- forkIO $ run tStatus
  return $ readTVarIO tStatus
