{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module:      Y2023.D04
Description: Advent of Code 2023 Day 04 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2023 day 04 set of problems for <adventofcode.com>.
-}
module Y2023.D04 where

import Data.Attoparsec.Text hiding (take)
import Data.Text (Text)
import Data.List (intersect)
import Data.Monoid (Sum(..))
import Data.Either.Utils (fromRight)
import qualified Data.Vector as V

-- |Solve for part A
part4A :: Text -> Int
part4A = getSum . foldMap (Sum . score) . parse4

score :: (Num a, Integral b) => b -> a
score n | n == 0 = 0
        | otherwise = 2 ^ pred n

-- |Solve for part B
part4B :: Text -> Int
part4B input = go 0 (zip [0..] allCards)
  where
    go !acc [] = acc
    go !(succ -> acc) ((succ -> idx, matches):cards)
      = go acc (zip [idx..] (V.toList (V.slice idx matches vecCards)) ++ cards)
    allCards = parse4 input
    vecCards = V.fromList allCards

parse4 :: Text -> [Int]
parse4 = fromRight . parseOnly parser
  where
    parser = map length <$> line `sepBy` endOfLine
    line = intersect <$> (header *> numbers) <*> (skipSpace *> "|" *> skipSpace *> numbers)
    numbers = (decimal :: Parser Int) `sepBy` skipSpace
    header = "Card" *> skipSpace *> (decimal :: Parser Int) *> ":" *> skipSpace
