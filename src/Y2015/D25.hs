{-|
Module:      Y2015.D25
Description: Advent of Code Day 25 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 25 set of problems for <adventofcode.com>.

Implements a simple function to return the machine code at a position
indicated by plaintext input for a row and column.
-}

module Y2015.D25 (manualCodeFrom) where

type Point = (Int, Int)

-- |Return the manual code found at the indicated row and column
-- |from human-readable input.
manualCodeFrom :: String  -- ^ Plaintext instruction input.
               -> Integer -- ^ Numerical code found at the indicated position.
manualCodeFrom = (!!) manualSeq . toPos . toPoint
    where toPos (x, y) = (i - 1) * (i - 2) `div` 2 + i - x - 1
                       where i = x + y

toPoint :: String -> Point
toPoint = go . words
    where go s = (grab 15, grab 17)
                 where grab = read . init . (!!) s

manualSeq :: [Integer]
manualSeq = iterate f 20151125 where
    f = flip mod 33554393 . (*) 252533
