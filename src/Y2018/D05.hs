{-|
Module:      Y2018.D05
Description: Advent of Code Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 05 set of problems for <adventofcode.com>.
-}
module Y2018.D05
  ( react
  , reactBest
  )
where

import Data.Char (toLower, toUpper)
import Data.List (nub)

reactBest :: String -> Int
reactBest input = minimum $ map react $ map inputWithout candidates
  where candidates = nub $ map toUpper input
        inputWithout c = filter (not . sameLetter c) input
        sameLetter x y = (toUpper x) == y || (toLower x) == y

react :: String -> Int
react = length . foldr collapse ""
  where collapse piece l@(x:xs) | reactive piece x = xs
                                | otherwise = [piece] ++ l
        collapse piece [] = [piece]
        reactive a b = a /= b && toUpper a == toUpper b
