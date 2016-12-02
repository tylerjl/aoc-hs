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
import qualified Data.Set as Set

data Direction = North
               | East
               | South
               | West
               deriving (Enum)

type Coordinates = (Int, Int)

blockDistance :: String -> Int
blockDistance = blocks
              . foldl' travel (0, 0)
              . mapRoute

visitedTwice :: String -> Maybe Int
visitedTwice = navigate (0,0) Set.empty
             . mapRoute
    where navigate point set (path:xs)
              | Set.member point' set = Just $ blocks point'
              | otherwise = navigate point' (Set.insert point' set) xs
              where point' = travel point path
          navigate _ _ _ = Nothing

mapRoute :: String -> [Direction]
mapRoute = toRoute North
         . map (toPath . filter  (not . (==) ','))
         . words
         where toRoute orientation ((lr, distance):xs) =
                   replicate distance orientation' ++ toRoute orientation' xs
                   where orientation' = turn lr orientation
               toRoute _ [] = []

blocks :: Coordinates -> Int
blocks (x, y) = abs x + abs y

travel :: Coordinates -> Direction -> Coordinates
travel (x, y) d = case d of
                    North -> (x, succ y)
                    East -> (succ x, y)
                    South -> (x, pred y)
                    West -> (pred x, y)

toPath :: String -> (Char, Int)
toPath (direction:steps) = (direction, read steps)
toPath [] = (' ', 0)

turn :: Char -> Direction -> Direction
turn 'R' West = North
turn 'L' North = West
turn 'R' d = succ d
turn 'L' d = pred d
turn _ d = d
