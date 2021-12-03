{-|
Module:      Y2021.D03
Description: Advent of Code 2021 Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 03 set of problems for <adventofcode.com>.
-}
module Y2021.D03 where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts
import Data.List (transpose, group, sort, sortOn, groupBy)
import Data.Function (on)

-- |For clearer function signatures.
type Bit  = Int
-- |Also, for clearer function signatures.
type Bits = [Bit]

-- |Accept a list of bits and multiply the common indexes.
part3A :: Text -> Float
part3A (toBits -> input)
  = bitsToDec (commonOn id input)
  * bitsToDec (commonOn Down input)

-- |Accept a list of bits and multiply the matching value with the matching
-- common and least-common bits, respectively.
part3B :: Text -> Float
part3B (toBits -> input) = oxygen * co2
  where oxygen = pinpoint 0 input id
        co2 = pinpoint 0 input Down
        pinpoint _ [] _ = error "couldn't find matching bits"
        pinpoint _ [x] _ = bitsToDec x
        pinpoint idx bits f = let common = commonOn f bits
          in pinpoint (succ idx) (keepAt bits (common !! idx) idx) f

-- |Keep bit arrays that have a particular bit set at a particular index.
keepAt :: [Bits] -> Bit -> Int -> [Bits]
keepAt bitsList bit idx = filter ((==) bit . flip (!!) idx) bitsList

-- |Convert a bit array to a decimal value.
bitsToDec :: Bits -> Float
bitsToDec = go 0 . reverse
  where go n (x:xs)
          | x == 1 = 2 ** n + go (succ n) xs
          | otherwise = go (succ n) xs
        go _ []     = 0

-- |Big fugly function to take a list of bits and return the common bit set in
-- each position.
commonOn :: Ord b => (Int -> b) -> [Bits] -> Bits
commonOn f =
  map ( head . head . head . map (sortOn (f . head))
        . groupBy ((==) `on` length) . sortOn (f . length) . group . sort)
  -- Turn rows into lists of each position
  . transpose

-- |Input parser to accept newline-separated bits
toBits :: Text -> [Bits]
toBits = map (map toBit . toList) . T.lines

-- |Fail-fast char-to-int parser.
toBit :: Char -> Int
toBit '0' = 0
toBit '1' = 1
toBit  _  = error "unknown value"

sample3 :: Text
sample3 = T.unlines
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
