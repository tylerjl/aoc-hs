{-|
Module:      Y2015.D17
Description: Advent of Code Day 17 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 17 set of problems for <adventofcode.com>.
-}

module Y2015.D17 (filledAmong, minFilledAmong) where

import Data.Function (on)
import Data.List (subsequences, minimumBy)

-- |Finds how many combinations of containers can fit a volume of nog
filledAmong :: Int    -- ^ Length
            -> String -- ^ List of container sizes
            -> Int    -- ^ Number of combinations
filledAmong t = containersBy length t . c
    where c = map read . lines

-- |Finds how many combinations of containers the ideal combination can
-- |fit a volume of nog
minFilledAmong :: Int    -- ^ Length
               -> String -- ^ List of container sizes
               -> Int    -- ^ Number of combinations
minFilledAmong t s =  containersBy (length . filter ((==) minFilled . length)) t c
    where c         = map read $ lines s
          minFilled = containersBy (length . minimumBy (compare `on` length))  t c

containersBy :: ([[Int]] -> a) -> Int -> [Int] -> a
containersBy f t s = f [x | x <- subsequences s, sum x == t]
