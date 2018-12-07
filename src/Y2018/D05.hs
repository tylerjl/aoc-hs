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

import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.List (groupBy, maximumBy, nub)
import Data.Ord  (comparing)

reactBest :: String -> Int
reactBest input = minimum $ map react $ map inputWithout candidates
  where candidates = nub $ map toUpper input
        inputWithout c = filter (not . sameLetter c) input
        sameLetter x y = (toUpper x) == y || (toLower x) == y

react :: String -> Int
react input | length (maximumBy (comparing length) (groupBy opposites input)) > 1 =
              react $ concat $ dropFirstCouple $ groupBy opposites input
            | otherwise = length input

dropFirstCouple :: [String] -> [String]
dropFirstCouple (x:xs) | length x > 1 = [(drop 2 x)] ++ xs
                       | otherwise    = [x] ++ dropFirstCouple xs
dropFirstCouple [] = []

opposites :: Char -> Char -> Bool
opposites a b = ((isUpper a && isLower b) || (isLower a && isUpper b)) && (toUpper a == toUpper b)
