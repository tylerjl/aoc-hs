module Y2021.D03 where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts
import Data.List (transpose, group, sort, sortOn, groupBy)
import Data.Function (on)

type Bit  = Int
type Bits = [Bit]

part3A :: Text -> Float
part3A (toBits -> input)
  = bitsToDec (commonOn id input)
  * bitsToDec (commonOn Down input)

part3B :: Text -> Float
part3B (toBits -> input) = oxygen * co2
  where oxygen = pinpoint 0 input id
        co2 = pinpoint 0 input Down
        pinpoint _ [] _ = error "couldn't find matching bits"
        pinpoint _ [x] _ = bitsToDec x
        pinpoint idx bits f = let common = commonOn f bits
          in pinpoint (succ idx) (keepAt bits (common !! idx) idx) f

keepAt :: [Bits] -> Bit -> Int -> [Bits]
keepAt bitsList bit idx = filter ((==) bit . flip (!!) idx) bitsList

bitsToDec :: Bits -> Float
bitsToDec = go 0 . reverse
  where go n (x:xs)
          | x == 1 = 2 ** n + go (succ n) xs
          | otherwise = go (succ n) xs
        go _ []     = 0

commonOn :: Ord b => (Int -> b) -> [[Int]] -> [Int]
commonOn f =
  map ( head . head . head . map (sortOn (f . head))
        . groupBy ((==) `on` length) . sortOn (f . length) . group . sort)
  -- Turn rows into lists of each position
  . transpose

toBits :: Text -> [Bits]
toBits = map (map toBit . toList) . T.lines

toBit :: Char -> Int
toBit '0' = 0
toBit '1' = 1
toBit  _  = error "unknown value"
