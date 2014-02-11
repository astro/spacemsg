{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Network.Protocol.NetSNMP
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import System.ZMQ3.Monadic


type Src = RawOID -> IO (Maybe SnmpResult)
type SrcT a = ReaderT Src IO a

src :: RawOID -> SrcT (Maybe SnmpResult)
src oid = ask >>= \f -> liftIO $ f oid

getExt :: SrcT [(BC.ByteString, JSON.Value)]
getExt = do
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
                       Just (name, asnValueToJSON value)
                     _ ->
                       Nothing
                 ) $
        zip names values

  return namesValues
  
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
      liftIO $ putStrLn "recvd, running"
      objs <- liftIO $ run ["dogbert.hq.c3d2.de", "ratbert.hq.c3d2.de"]
      liftIO $ putStrLn $ "got " ++ show objs
      send' rep [] $ JSON.encode $ JSON.toJSON objs
  
run :: [BC.ByteString] -> IO JSON.Value
run hosts = do
  initialize
  let src' :: BC.ByteString -> Src
      src' host oid =
        snmpGet snmp_version_1 host snmpCommunity oid >>=
        either (\e -> do
                   putStrLn e
                   return Nothing
               ) (return . Just)

      snmpCommunity :: Community
      snmpCommunity = "public"

  JSON.object <$>
    foldM (\obj host -> do
              obj' <- map (\(k, v) ->
                            decodeUtf8 (BC.concat [host, ":", k]) .= v
                          ) <$> runReaderT getExt (src' host)
              return $ obj ++ obj'
        ) [] hosts


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
