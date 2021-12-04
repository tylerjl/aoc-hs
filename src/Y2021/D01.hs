{-|
Module:      Y2021.D01
Description: Advent of Code 2021 Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 01 set of problems for <adventofcode.com>.
-}
module Y2021.D01 where

import Data.Foldable (foldl')
import Data.Text (Text)
import Witch

import qualified Data.Text as T

-- |First attempt at part a, not optimized.
partA :: Text -> Int
partA = stepwise . asInts

-- |Part a using high-level zip operations.
partAZip :: Text -> Int
partAZip = length . compareAdj . asInts

-- |Part a using simple recursion (maybe smaller big-O?)
partARecur :: Text -> Int
partARecur = go . asInts
  where go (x:y:zs) = (if x < y then 1 else 0) + go (y:zs)
        go _ = 0

-- |Part b first attempt, simple approach
partB :: Text -> Int
partB = stepwise . toWindows . asInts

-- |Part b using highl-elevel zips
partBZip :: Text -> Int
partBZip = length . compareAdj . map trisum . (zip3 <*> tail <*> tail . tail) . asInts

-- |Utility to transform a list into tuples of adjacent values.
compareAdj :: [Int] -> [(Int, Int)]
compareAdj = filter (uncurry (<)) . (zip <*> tail)

-- |Sum all values of a three-tuple
trisum :: Num a => (a, a, a) -> a
trisum (a, b, c) = a + b + c

-- |A fold sum of the structure for parsed values.
stepwise :: [Int] -> Int
stepwise = snd . foldl' steps (Nothing, 0)
  where steps :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
        steps (Nothing,   acc) n = (Just n, acc)
        steps (Just prev, acc) n
          | prev < n  = (Just n, succ acc)
          | otherwise = (Just n, acc)

-- |Consolidate a list into a summed, shifting window.
toWindows :: [Int] -> [Int]
toWindows (w:x:y:zs) = (w+x+y) : toWindows (x:y:zs)
toWindows _ = []

-- |Unsafe-ish `Text` traversal to transform into a list of `Int`s.
asInts :: Text -> [Int]
asInts = map (read . into @String) . T.lines

d1sample :: Text
d1sample = T.unlines $ map (into @Text . show)
  ([199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int])
