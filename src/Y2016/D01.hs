{-|
Module:      Y2016.D01
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}
module Y2016.D01
  ( blockDistance
  , visitedTwice
  ) where

import Data.List (foldl')

data Direction = North
               | East
               | South
               | West
               deriving Enum

type Coordinates = (Int, Int)

blockDistance :: String -> Int
blockDistance = (\(x, y) -> abs x + abs y)
              . snd
              . foldl' travel (North, (0, 0))
              . map (toPath . filter  (not . (==) ','))
              . words

visitedTwice :: String -> Int
visitedTwice _ = 0

travel :: (Direction, Coordinates) -> (Char, Int) -> (Direction, Coordinates)
travel (dir, c) (lr, steps) =
    (dir', walk c dir' steps)
    where dir' = turn lr dir

toPath :: String -> (Char, Int)
toPath (direction:steps) = (direction, read steps)
toPath [] = (' ', 0)

walk :: Coordinates -> Direction -> Int -> Coordinates
walk (x, y) d l =
    case d of
      North -> (x, y + l)
      East -> (x + l, y)
      South -> (x, y - l)
      West -> (x - l, y)

turn :: Char -> Direction -> Direction
turn 'R' West = North
turn 'L' North = West
turn 'R' d = succ d
turn 'L' d = pred d
turn _ d = d
