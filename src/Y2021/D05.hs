{-# LANGUAGE TupleSections #-}
{-|
Module:      Y2021.D05
Description: Advent of Code 2021 Day 05 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 05 set of problems for <adventofcode.com>.
-}
module Y2021.D05 where

import Data.Either.Utils (fromRight)
import Data.Text (Text)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Attoparsec.Text

-- |Makes some signatures easier to read
type Point = (Int, Int)
-- |Makes some signatures easier to read
type Ray = [(Point, Point)]

-- |Solve part A
part5A :: Text -> Int
part5A = solve5 (filter noAngle)
  where noAngle ((x1, y1), (x2, y2))
          = x1 == x2 || y1 == y2

-- |Solve part A, but with a HashMap
part5AHM :: Text -> Int
part5AHM = solve5HM (filter noAngle)
  where noAngle ((x1, y1), (x2, y2))
          = x1 == x2 || y1 == y2

-- |Solve part B
part5B :: Text -> Int
part5B = solve5 id

-- |Solve part B, but with a HashMap
part5BHM :: Text -> Int
part5BHM = solve5HM id

-- |Higher-order function solution to share parts A and B.
solve5 :: (Ray -> Ray) -> Text -> Int
solve5 f =
  M.size . M.filter (> 1) . M.fromListWith mappend .
  concatMap (uncurry lineTo) . f . parseVents

-- |Same as `solve5`, but using HashMap.
solve5HM :: (Ray -> Ray) -> Text -> Int
solve5HM f =
  HM.size . HM.filter (> 1) . HM.fromListWith mappend .
  concatMap (uncurry lineTo) . f . parseVents

-- |Accept a start and end point and return a list of points that draw a line to
-- the endpoint. Note that this doesn't work for anything other than vertical,
-- horizontal, and 45deg.
lineTo :: Point -> Point -> [(Point, Sum Int)]
lineTo (x1, y1) (x2, y2) = zipWith (curry (, Sum 1)) (range x1 x2) (range y1 y2)
  where range p1 p2 | p1 == p2  = repeat p1
                    | otherwise = p1 ~~ p2

-- |Messing around with a custom operator for ranges that can handle both up-to
-- and down-to.
infixl 5 ~~
(~~) :: Int -> Int -> [Int]
a ~~ b | a <= b    = [a .. b]
       | otherwise = reverse $ b ~~ a

-- |Parse puzzle input into simple pairs of pairs of points.
parseVents :: Text -> Ray
parseVents = fromRight . parseOnly parser
  where
    parser = line `sepBy1` endOfLine <* atEnd
    line  = (,) <$> point <* string " -> " <*> point
    point = (,) <$> decimal <* char ',' <*> decimal
