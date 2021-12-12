{-# LANGUAGE TupleSections #-}
{-|
Module:      Y2021.D05
Description: Advent of Code 2021 Day 05 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 05 set of problems for <adventofcode.com>.
-}
module Y2021.D05 where

import Data.Attoparsec.Text
import Data.Either.Utils (fromRight)
import Data.Text (Text)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid

-- |Makes some signatures easier to read
type Point = (Int, Int)
-- |Makes some signatures easier to read
type Line = (Point, Point)

-- |Solve part A
part5A :: Text -> Int
part5A = M.size . solve5 (filter part5APredicate) . parseVents

part5APredicate :: Line -> Bool
part5APredicate ((x1, y1), (x2, y2))
  = x1 == x2 || y1 == y2

-- |Solve part B
part5B :: Text -> Int
part5B = M.size . solve5 id . parseVents

-- |Higher-order function solution to share parts A and B.
solve5 :: ([Line] -> [Line]) -> [Line] -> HashMap Point (Sum Int)
solve5 f unfilteredLines =
  M.filter (> (1 :: Sum Int)) $ foldl' intersectsWith M.empty lines'
  where
    lines' = f unfilteredLines
    intersectsWith allPoints line
      = M.unionWith mappend allPoints $ M.fromList $ map (, Sum 1) intersections
      where intersections = mapMaybe (intersection' line) lines'
            intersection' testLine allLine
              | testLine == allLine = Nothing
              | otherwise = intersection testLine allLine

-- |Parse puzzle input into simple pairs of pairs of points.
parseVents :: Text -> [Line]
parseVents = fromRight . parseOnly parser
  where
    parser = line `sepBy1` endOfLine <* atEnd
    line  = (,) <$> point <* string " -> " <*> point
    point = (,) <$> decimal <* char ',' <*> decimal

-- |I'm not smart enough to figure this one out, but it's a formula to come up
-- with the intersection between two xy lines.
intersection :: Line -> Line -> Maybe Point
intersection a@((x1, y1), (x2, y2)) b@((x1', y1'), (x2', y2'))
  | div' == 0 = Nothing
  | otherwise =
    let d = (uncurry det a, uncurry det b)
    in pure (det d xdiff `div` div', det d ydiff `div` div')
  where
    xdiff = (x1 - x2, x1' - x2')
    ydiff = (y1 - y2, y1' - y2')
    det (a1, b1) (a2, b2) = a1 * b2 - b1 * a2
    div' = det xdiff ydiff
