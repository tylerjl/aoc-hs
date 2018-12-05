{-# LANGUAGE RankNTypes #-}
{-|
Module:      Y2018.D02
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}
module Y2018.D02
  ( boxID
  , checksum
  ) where

import Data.List (group, sort)

boxID :: String -> Maybe String
boxID = findBox . lines
-- boxID = findBox . filter (\s -> has 2 s || has 3 s) . lines

findBox :: forall b. Eq b => [[b]] -> Maybe [b]
findBox (box:boxes) = case matchingBoxes box boxes of
                        [] -> findBox boxes
                        (match:_) -> Just $ fst $ unzip match
findBox [] = Nothing

matchingBoxes :: forall a. Eq a => [a] -> [[a]] -> [[(a, a)]]
matchingBoxes box boxes = map (filter pair) $ filter ((==) 1 . length . unmatched) $ map (zip box) boxes
  where unmatched = filter (not . pair)
        pair (a, b) = a == b

checksum :: String -> Int
checksum boxes = product $ map length [(doubles boxes'), (triples boxes')]
  where boxes' = lines boxes

doubles :: [String] -> [String]
doubles = filter (has 2)

triples :: [String] -> [String]
triples = filter (has 3)

has :: Ord a => Int -> [a] -> Bool
has n = any ((==) n . length) . group . sort
