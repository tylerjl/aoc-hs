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
import Text.Parsec
import Y2015.Util (intParser', regularParse')

import qualified Data.Map as M

-- |Makes some signatures easier to read
type Point = (Int, Int)
-- |Makes some signatures easier to read
type Ray = [(Point, Point)]

-- |Solve part A
part5A :: Text -> Int
part5A = solve5 (filter noAngle)
  where noAngle ((x1, y1), (x2, y2))
          = x1 == x2 || y1 == y2

-- |Solve part B
part5B :: Text -> Int
part5B = solve5 id

-- |Higher-order function solution to share parts A and B.
solve5 :: (Ray -> Ray) -> Text -> Int
solve5 f =
  M.size . M.filter (> (1 :: Int)) . M.fromListWith (+) .
  concatMap (map (, 1) . uncurry lineTo) . f . parseVents

-- |Accept a start and end point and return a list of points that draw a line to
-- the endpoint. Note that this doesn't work for anything other than vertical,
-- horizontal, and 45deg.
lineTo :: Point -> Point -> [Point]
lineTo (x1, y1) (x2, y2) = zip (range x1 x2) (range y1 y2)
  where range p1 p2 | p1 == p2  = repeat p1
                    | otherwise = p1 ~~ p2

-- |Messing around with a custom operator for ranges that can handle both up-to
-- and down-to.
(~~) :: Int -> Int -> [Int]
a ~~ b | a <= b    = [a .. b]
       | otherwise = reverse $ b ~~ a

-- |Should be fun!
infixl 5 ~~

-- |Parse puzzle input into simple pairs of pairs of points.
parseVents :: Text -> Ray
parseVents = fromRight . regularParse' (line `endBy1` newline <* eof)
  where
    line  = (,) <$> point <* string " -> " <*> point
    point = (,) <$> intParser' <* char ',' <*> intParser'
