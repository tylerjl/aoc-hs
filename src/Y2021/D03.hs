module Y2021.D03 where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts
import Data.List (transpose, group, sort)
import Data.List.Extra (maximumOn, minimumOn)

type Bit  = Int
type Bits = [Bit]

part3A :: Text -> Float
part3A (toBits -> input)
  = bitsToDec (commonOn maximumOn input)
  * bitsToDec (commonOn minimumOn input)

part3B :: Text -> Int
part3B = error "not implemented"

keepAt :: [Bits] -> Bit -> Int -> [Bits]
keepAt bitsList bit idx = filter ((==) bit . flip (!!) idx) bitsList

bitsToDec :: Bits -> Float
bitsToDec = go 0 . reverse
  where go n (x:xs)
          | x == 1 = 2 ** n + go (succ n) xs
          | otherwise = go (succ n) xs
        go _ []     = 0

commonOn :: ((Bits -> Int) -> [Bits] -> Bits) -> [Bits] -> Bits
commonOn f = map (head . f length . group . sort) . transpose

toBits :: Text -> [Bits]
toBits = map (map toBit . toList) . T.lines

toBit :: Char -> Int
toBit '0' = 0
toBit '1' = 1
toBit  _  = error "unknown value"
