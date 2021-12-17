{-|
Module:      Y2021.D16
Description: Advent of Code 2021 Day 16 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 16 set of problems for <adventofcode.com>.
-}
module Y2021.D16
  ( parse16
  , part16A
  , part16B
  ) where

import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Either.Utils       (fromRight)
import Data.Foldable
import Data.Monoid
import Data.Text               (Text)

import qualified Data.Text as T
import qualified GHC.Exts  as L

-- |Solution to part A
part16A :: Text -> Int
part16A = sumVersion . parse16

sumVersion :: Packet -> Int
sumVersion (PacketLit v _) = v
sumVersion (PacketOp v _ ps) = v + getSum (foldMap (Sum . sumVersion) ps)

-- |Solution to part B
part16B :: Text -> Int
part16B = decodePacket . parse16

decodePacket :: Packet -> Int
decodePacket (PacketLit _ n) = n
decodePacket (PacketOp _ 0 ps) = foldl' (\acc -> (+) acc . decodePacket) 0 ps
decodePacket (PacketOp _ 1 ps) = foldl' (\acc -> (*) acc . decodePacket) 1 ps
decodePacket (PacketOp _ 2 ps) = minimum (map decodePacket ps)
decodePacket (PacketOp _ 3 ps) = maximum (map decodePacket ps)
decodePacket (PacketOp _ 5 [p1, p2])
  | decodePacket p1 > decodePacket p2 = 1
  | otherwise = 0
decodePacket (PacketOp _ 6 [p1, p2])
  | decodePacket p1 < decodePacket p2 = 1
  | otherwise = 0
decodePacket (PacketOp _ 7 [p1, p2])
  | decodePacket p1 == decodePacket p2 = 1
  | otherwise = 0
decodePacket PacketOp {} = error "unknown operator packet"

type Version = Int
type PType = Int
data Packet
  = PacketLit Version Int
  | PacketOp Version PType [Packet]
  deriving Show

-- |Parse.
parse16 :: Text -> Packet
parse16 = fromRight . parseOnly parser . hexToBin
  where
    parser = packet <* atEnd
    packet = packetLit <|> packetOp

    packetLit = PacketLit <$> bitN 3 <* string "100" <*> (asInt . concat <$> litNumber)
    litNumber = litQuad
    litQuad = mappend <$> many litInit <*> ((: []) <$> litTail)
    litInit = char '1' *> count 4 bit
    litTail = char '0' *> count 4 bit

    packetOp = PacketOp <$> bitN 3 <*> bitN 3 <*> (lenPacket <|> numPacket)
    lenPacket = do
      _ <- char '0'
      len <- bitN 15
      parseOf (P.take len) (many1 packet)
    numPacket = do
      _ <- char '1'
      packets <- bitN 11
      count packets packet

    bitN n = asInt <$> count n bit
    asInt = convert . reverse
    bit = read . (: []) <$> binary
    binary = char '0' <|> char '1'

constP :: Parser a -> Text -> Parser a
constP p t = case parseOnly p t of
  Left _ -> empty
  Right a -> return a

parseOf :: Parser Text -> Parser a -> Parser a
parseOf ptxt pa = bothParse
  where
    bothParse = ptxt >>= constP pa

hexToBin :: Text -> Text
hexToBin = T.concat . map hexToBits  . L.toList

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

hexToBits :: Char -> Text
hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"
hexToBits _ = ""
