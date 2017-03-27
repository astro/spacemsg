module Collectd.Packet where

import Data.Word
import Data.Int
import Data.Bits (shiftR)
import Control.Monad
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Serialize.Get
import Data.Serialize.IEEE754

data Frame = Frame Word16 B.ByteString
  deriving (Show)

decode :: B.ByteString -> Either String [Packet]
decode = runGet $
         concatMap (maybe [] (: []) . decodePacket) <$> frames

frames :: Get [Frame]
frames = do
  isEmpty' <- isEmpty
  case isEmpty' of
    True ->
      return []
    False -> do
      frame' <- frame
      (frame' :) <$> frames

frame :: Get Frame
frame = do
  pType <- getWord16be
  pLen <- fromIntegral <$> getWord16be
  case pLen - 4 of
    pLen' | pLen' >= 0 ->
            Frame pType <$>
            getBytes pLen'
    _ -> fail "Short frame"


data Packet = Host Text
            | Time Word64
            | Interval Word64
            | Plugin Text
            | PluginInstance Text
            | Type Text
            | TypeInstance Text
            | Values [Value]
            | Message Text
            | Severity Word64
            deriving (Show)

data Value = Counter Word64
           | Gauge Double
           | Derive Int64
           | Absolute Word64
           | OtherValueType Word8 B.ByteString
           deriving (Show)

decodePacket :: Frame -> Maybe Packet
decodePacket (Frame pType pData) = decodePacket' pType pData

decodePacket' :: Word16 -> B.ByteString -> Maybe Packet
decodePacket' 0 = Just . Host . string
decodePacket' 1 = (Time <$>) . number
decodePacket' 8 = (Time . (`shiftR` 30) <$>) . number
decodePacket' 2 = Just . Plugin . string
decodePacket' 3 = Just . PluginInstance . string
decodePacket' 4 = Just . Type . string
decodePacket' 5 = Just . TypeInstance . string
decodePacket' 6 = either (const Nothing) (Just . Values) .
                  runGet values
  where
    values = do
      count <- getWord16be
      vTypes <- forM [1..count] $
        const getWord8
      forM vTypes value

    value vType = do
      case vType of
        0 -> Counter <$> getWord64be
        1 -> Gauge <$> getFloat64le
        2 -> Derive <$> getInt64be
        3 -> Absolute <$> getWord64be
        _ -> OtherValueType vType <$>
             (remaining >>= getBytes)
decodePacket' 7 = (Interval <$>) . number
decodePacket' 9 = (Interval . (`shiftR` 30) <$>) . number
decodePacket' 0x100 = Just . Message . string
decodePacket' 0x101 = (Severity <$>) . number
decodePacket' _ = const Nothing


string :: B.ByteString -> Text
string = decodeUtf8 . B.takeWhile (/= 0)

number :: B.ByteString -> Maybe Word64
number = either (const Nothing) Just . runGet getWord64be
