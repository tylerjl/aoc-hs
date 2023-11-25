{-|
Module:      Y2016.D13
Description: Advent of Code Day 13 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 13 set of problems for <adventofcode.com>.
-}
module Y2016.D13 (floodOffice, officePath) where

import Algorithm.Search (dijkstra)
import Data.Bits (popCount)
import Data.Set (Set)
import qualified Data.Set as S
import Y2015.Util ((<&&>))

type Point = (Int, Int)
data Tile = Space | Wall deriving Show

officePath :: Int -> Point -> Maybe Int
officePath seed target = fst
  <$> dijkstra (neighbors seed) (const . const 1) (== target) (1, 1)
 
floodOffice :: Int -> Int -> Set Point
floodOffice seed maxSteps = go 0 S.empty (1, 1)
  where
    go :: Int -> Set Point -> Point -> Set Point
    go steps seen node
      | steps > maxSteps || node `S.member` seen = S.empty
      | otherwise = S.unions
                    $ S.singleton node
                    : map (go (succ steps) (node `S.insert` seen)) (neighbors seed node)

neighbors :: Int -> Point -> [Point]
neighbors seed point = filter (inBounds <&&> (open . tileAt seed))
  $ adjacentTo point
  where
    open Space = True
    open Wall = False
    inBounds (x, y) = x >= 0 && y >= 0

adjacentTo :: Point -> [Point]
adjacentTo (x, y) =
  [             (x, y - 1)
  , (x - 1, y),            (x + 1, y)
  ,             (x, y + 1)
  ]

tileAt :: Int -> Point -> Tile
tileAt seed (x, y) = if even (popCount q) then Space else Wall
  where
    q = seed + ((x * x) + (3 * x) + (2 * (x * y)) + y + (y * y))
