{-|
Module:      Y2015.D05
Description: Advent of Code Day 05 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 05 set of problems for <adventofcode.com>.
-}
module Y2015.D05
  ( clean
  , isNice
  , isNicer
  , thriceVoweled
  , twiceRow
  ) where

import Y2015.Util ((<&&>))

import Data.List (group, isInfixOf)

-- |Whether a given string is "nice" per the spec.
isNicer
  :: String -- ^ Test input string
  -> Bool -- ^ Whether string is nice
isNicer = repeatedPair <&&> repeatedBetween

repeatedPair :: String -> Bool
repeatedPair (x:y:zs)
  | [x, y] `isInfixOf` zs = True
  | otherwise = repeatedPair (y : zs)
repeatedPair _ = False

repeatedBetween :: String -> Bool
repeatedBetween (x:y:z:zs)
  | x == z = True
  | otherwise = repeatedBetween (y : z : zs)
repeatedBetween _ = False

-- |Predicate to determine whether a given string is "nice".
isNice
  :: String -- ^ Test input string.
  -> Bool -- ^ Whether the given input string is nice.
isNice = clean <&&> thriceVoweled <&&> twiceRow

-- |Predicate to determine whether a string eschews forbidden strings.
clean
  :: String -- ^ Input string.
  -> Bool -- ^ Whether the string is clean.
clean = not . flip any forbiddenStrings . flip isInfixOf

forbiddenStrings :: [String]
forbiddenStrings = ["ab", "cd", "pq", "xy"]

-- |Predicate to determine whether a given string contains two letters
-- |in a row.
twiceRow
  :: String -- ^ Input string to test.
  -> Bool -- ^ Whether the given string passes the predicate.
twiceRow = any ((> 1) . length) . group

-- |Predicate to determine whether the given string contains at least three
-- |vowels.
thriceVoweled
  :: String -- ^ Input string to test.
  -> Bool -- ^ Whether the string passes the predicate.
thriceVoweled = (> 2) . length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem ("aeiou" :: String)
