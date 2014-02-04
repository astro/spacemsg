{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Network.Protocol.NetSNMP
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Maybe
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import System.ZMQ3.Monadic


type Src = RawOID -> IO (Maybe SnmpResult)
type SrcT a = ReaderT Src IO a

src :: RawOID -> SrcT (Maybe SnmpResult)
src oid = ask >>= \f -> liftIO $ f oid

getExt :: [(B.ByteString, T.Text, T.Text)] -> SrcT [JSON.Value]
getExt config = do
  let collect i j = do
        mResult <- src $ extOid ++ [i, j]
        case mResult of
          Nothing ->
            return []
          Just result -> do
            let result' = value result
                rest = collect i $ j + 1
            (result' :) <$> rest
  names <- collect 2 1
  values <- collect 100 1

  let namesValues =
        mapMaybe (\nameValue ->
                   case nameValue of
                     (OctetString name _, value) ->
                       Just (name, value)
                     _ ->
                       Nothing
                 ) $
        zip names values

  return $
    flip mapMaybe config $ \(name, name', unit) ->
    do value <- name `lookup` namesValues
       return $ JSON.object
         [ "name" .= JSON.toJSON name',
           "value" .= asnValueToJSON value,
           "unit" .= JSON.toJSON unit
         ]
  
  where extOid :: RawOID
        extOid = [1, 3, 6, 1, 4, 1, 2021, 8, 1]

main :: IO ()
main =
  runZMQ $ do
    rep <- socket Rep
    setIpv4Only False rep
    bind rep "tcp://*:5555"
    forever $ do
      _ <- receive rep
      objs <- liftIO $ run config
      send' rep [] $ JSON.encode $ JSON.toJSON objs
  
  where config =
          [ ("dogbert.hq.c3d2.de", getExt [ ("wlan0-stations", "dogbert 2.4 GHz", "WiFi stations")
                                          , ("wlan1-stations", "dogbert 5 GHz", "WiFi stations")
                                          , ("wlan1-1-stations", "dogbert 5 GHz extra", "WiFi stations")
                                          ])
          , ("ratbert.hq.c3d2.de", getExt [ ("wlan0-stations", "ratbert 2.4 GHz", "WiFi stations")
                                          ])
          ]

run :: [(B.ByteString, SrcT [JSON.Value])] -> IO [JSON.Value]
run config = do
  initialize
  let src :: B.ByteString -> Src
      src host oid =
        snmpGet snmp_version_1 host snmpCommunity oid >>=
        either (\e -> do
                   putStrLn e
                   return Nothing
               ) (return . Just)

      snmpCommunity :: Community
      snmpCommunity = "public"

  foldM (\objs (host, action) ->
          (++ objs) <$> runReaderT action (src host)
        ) [] config


asnValueToJSON :: ASNValue -> JSON.Value
asnValueToJSON (Integer32 i) = JSON.toJSON i
asnValueToJSON (Integer64 i) = JSON.toJSON i
asnValueToJSON (Counter32 i) = JSON.toJSON i
asnValueToJSON (Counter64 i) = JSON.toJSON i
asnValueToJSON (Unsigned32 i) = JSON.toJSON i
asnValueToJSON (Unsigned64 i) = JSON.toJSON i
asnValueToJSON (Gauge32 i) = JSON.toJSON i
asnValueToJSON (Boolean b) = JSON.toJSON b
asnValueToJSON (IEEEFloat f) = JSON.toJSON f
asnValueToJSON (IEEEDouble d) = JSON.toJSON d
asnValueToJSON Null = JSON.Null


