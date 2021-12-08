{-|
Module:      Y2021.D01
Description: Advent of Code 2021 Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 01 set of problems for <adventofcode.com>.
-}
module Y2021.D01 where

import Data.Attoparsec.Text
import Data.Either.Utils (fromRight)
import Data.Foldable (foldl')
import Data.Text (Text)

-- |First attempt at part a, not optimized.
part1A :: Text -> Int
part1A = stepwise . asInts

-- |Part a using high-level zip operations.
part1AZip :: Text -> Int
part1AZip = length . compareAdj . asInts

-- |Part a using simple recursion (maybe smaller big-O?)
part1ARecur :: Text -> Int
part1ARecur = go . asInts
  where go (x:y:zs) = (if x < y then 1 else 0) + go (y:zs)
        go _ = 0

-- |Part b first attempt, simple approach
part1B :: Text -> Int
part1B = stepwise . toWindows . asInts

-- |Part b using highl-elevel zips
part1BZip :: Text -> Int
part1BZip = length . compareAdj . map trisum . (zip3 <*> tail <*> tail . tail) . asInts

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
asInts = fromRight . parseOnly parser
  where
    parser = decimal `sepBy1` endOfLine
