{-|
Module:      Y2018.D02
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}
module Y2018.D02
  ( checksum
  ) where

import Data.List (group, sort)

checksum :: String -> Int
checksum boxes = product $ map length [doubles, triples]
  where doubles = filter (has 2) boxes'
        triples = filter (has 3) boxes'
        boxes' = lines boxes
        has n = any ((==) n . length) . group . sort
