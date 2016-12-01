{-|
Module:      Y2015.D11
Description: Advent of Code Day 11 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 11 set of problems for <adventofcode.com>.
-}
module Y2015.D11
  ( rotate
  ) where

import Data.List (group, isInfixOf, tails)

import Y2015.Util ((<&&>))

alphabet :: String
alphabet = ['a' .. 'z']

meetsReqs :: String -> Bool
meetsReqs = hasPairs <&&> (not . forbidden) <&&> hasStraightFast

hasPairs :: String -> Bool
hasPairs = (1 <) . length . filter ((<) 1 . length) . group

forbidden :: String -> Bool
forbidden = any (`elem` "iol")

hasStraightFast :: String -> Bool
hasStraightFast = not . null . filterAsc . subSeqs
  where
    filterAsc = filter (`isInfixOf` alphabet)
    subSeqs = takeWhile ((== 3) . length) . map (take 3) . tails

increment :: String -> String
increment = reverse . step . reverse
  where
    step [] = []
    step [x]
      | x == 'z' = "aa"
      | otherwise = [succ x]
    step (x:xs)
      | x /= 'z' = succ x : xs
      | otherwise = 'a' : step xs

-- |Rotate a password within specific rules
rotate
  :: String -- ^ Starting password
  -> String -- ^ Next valid password
rotate = nextValid . increment
  where
    nextValid = head . filter meetsReqs . iterate increment
