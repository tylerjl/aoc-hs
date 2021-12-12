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
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text         (Text)

import qualified Data.HashMap.Strict as M

-- |Makes some signatures easier to read
type Point = (Int, Int)
-- |Makes some signatures easier to read
type Line = (Point, Point)
-- |Makes some signatures easier to read
type Points = HashMap Point (Sum Int)

-- |Solve part A
part5A :: Text -> Int
part5A = solve5 (filter part5APredicate)

part5APredicate :: Line -> Bool
part5APredicate ((x1, y1), (x2, y2))
  = x1 == x2 || y1 == y2

-- |Solve part B
part5B :: Text -> Int
part5B = solve5 id

-- |Higher-order function solution to share parts A and B.
solve5 :: ([Line] -> [Line]) -> Text -> Int
solve5 f =
  M.size .
  M.filter (> 1) . M.fromListWith mappend . concatMap lineTo . f . parseVents

-- |Accept a start and end point and return a list of points that draw a line to
-- the endpoint. Note that this doesn't work for anything other than vertical,
-- horizontal, and 45deg.
lineTo :: Line -> [(Point, Sum Int)]
lineTo ((x1, y1), (x2, y2)) = zipWith (curry (, Sum 1)) (range x1 x2) (range y1 y2)
  where range p1 p2 | p1 == p2  = repeat p1
                    | otherwise = p1 ~~ p2

-- |Messing around with a custom operator for ranges that can handle both up-to
-- and down-to.
infixl 5 ~~
(~~) :: Int -> Int -> [Int]
a ~~ b | a <= b    = [a .. b]
       | otherwise = reverse $ b ~~ a

-- |Parse puzzle input into simple pairs of pairs of points.
parseVents :: Text -> [Line]
parseVents = fromRight . parseOnly parser
  where
    parser = line `sepBy1` endOfLine <* atEnd
    line  = (,) <$> point <* string " -> " <*> point
    point = (,) <$> decimal <* char ',' <*> decimal
