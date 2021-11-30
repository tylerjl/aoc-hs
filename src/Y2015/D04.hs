{-|
Module:      Y2015.D04
Description: Advent of Code Day 04 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 04 set of problems for <adventofcode.com>.
-}
module Y2015.D04
  ( crack
  ) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as C

-- |Cracks password hash to retrieve original value
crack
  :: C.ByteString -- ^ Input hash
  -> Int -- ^ Number of leading zeroes
  -> Int -- ^ Original password value
crack prefix d = head $ dropWhile (not . validSuffix) [0 ..]
  where
    validSuffix = check . encode . hash . (prefix <>) . C.pack . show
    check = (>= d) . C.length . C.takeWhile (== '0')
