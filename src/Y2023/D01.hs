{-|
Module:      Y2023.D01
Description: Advent of Code 2023 Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2023 day 01 set of problems for <adventofcode.com>.
-}
module Y2023.D01 where

import Data.Maybe (mapMaybe)

-- |Solve for part A
part1A :: String -> Int
part1A = sum . map base10Tuple . mapMaybe intoPair . calibrateA

-- |Solve for part B
part1B :: String -> Int
part1B = sum . map base10Tuple . mapMaybe intoPair . calibrateB

-- |Turn a tuple into a two-digit number
base10Tuple :: (Int, Int) -> Int
base10Tuple (x, y) = (x * 10) + y

-- |Given a list of ints, try and turn it into a tuple of first and
-- last elements, with Nothing as the result if it can’t be done.
intoPair :: [Int] -> Maybe (Int, Int)
intoPair [] = Nothing
intoPair [x] = Just (x, x)
intoPair [x, y] = Just (x, y)
intoPair nums = Just (head nums, last nums)

-- |Transform an input string into a list of all the digits found in
-- each line.
calibrateA :: String -> [[Int]]
calibrateA = map go . lines
  where
    go [] = []
    go (x:xs) | x `elem` ['0'..'9'] = read [x] : go xs
              | otherwise = go xs

-- |Transform an input string into a list of all the digits and
-- spelled numbers found in each line.
calibrateB :: String -> [[Int]]
calibrateB = map go . lines
  where
    go []                           = []
    go l@('o':'n':'e':_)            = 1 : go (tail l)
    go l@('t':'w':'o':_)            = 2 : go (tail l)
    go l@('t':'h':'r':'e':'e':_)    = 3 : go (tail l)
    go l@('f':'o':'u':'r':_)        = 4 : go (tail l)
    go l@('f':'i':'v':'e':_)        = 5 : go (tail l)
    go l@('s':'i':'x':_)            = 6 : go (tail l)
    go l@('s':'e':'v':'e':'n':_)    = 7 : go (tail l)
    go l@('e':'i':'g':'h':'t':_)    = 8 : go (tail l)
    go l@('n':'i':'n':'e':_)        = 9 : go (tail l)
    go (x:xs) | x `elem` ['0'..'9'] = read [x] : go xs
              | otherwise = go xs
