module Collectd.Listener where

import Control.Monad
import Data.Word
import Data.Text (Text)
import Network.Socket hiding (Type)
import Network.Socket.ByteString

import Collectd.Packet


runListener :: String -> String -> ([Datum] -> IO ()) -> IO ()
runListener host port handler = do
  addr:_ <- getAddrInfo Nothing (Just host) (Just port)
  print addr
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)

  forever $ do
    (buf, pktAddr) <- recvFrom sock 65536
    let ePackets = decode buf
    case ePackets of
      Left e -> putStrLn $ "Collectd recv from " ++ show pktAddr ++ " error: " ++ e
      Right packets -> handler $ packetsToData packets


-- | (host, plugin, pluginInstance, type, typeInstance)
type DatumPath = (Text, Text, Text, Text, Text)

data Datum = Datum {
  datumTime :: Word64,
  datumInterval :: Word64,
  datumPath :: DatumPath,
  datumValues :: [Value]
} deriving (Show)

packetsToData :: [Packet] -> [Datum]
packetsToData = fst .
                foldl process
                ([], (Nothing, Nothing, (Nothing, Nothing, Nothing, Nothing, Nothing)))

  where
    process (result, d) pkt =
      let (dTime, dInterval, dPath) = d
          (dHost, dPlugin, dPluginInstance, dType, dTypeInstance) = dPath
      in case pkt of
           Host host ->
            (result, (dTime, dInterval, (Just host, dPlugin, dPluginInstance, dType, dTypeInstance)))
           Time time ->
            (result, (Just time, dInterval, dPath))
           Interval interval ->
            (result, (dTime, Just interval, dPath))
           Plugin plugin ->
            (result, (dTime, dInterval, (dHost, Just plugin, dPluginInstance, dType, dTypeInstance)))
           PluginInstance pluginInstance ->
            (result, (dTime, dInterval, (dHost, dPlugin, Just pluginInstance, dType, dTypeInstance)))
           Type type' ->
            (result, (dTime, dInterval, (dHost, dPlugin, dPluginInstance, Just type', dTypeInstance)))
           TypeInstance typeInstance ->
            (result, (dTime, dInterval, (dHost, dPlugin, dPluginInstance, dType, Just typeInstance)))
           Values values ->
            maybe (result, d) id $ do
             path <- (,,,,) <$>
                     dHost <*>
                     dPlugin <*>
                     dPluginInstance <*>
                     dType <*>
                     dTypeInstance
             datum <- Datum <$>
                      dTime <*>
                      dInterval <*>
                      pure path <*>
                      pure values
             return (result ++ [datum], d)
           _ ->
            (result, (dTime, dInterval, dPath))
