{-|
Module:      Y2015.D03
Description: Advent of Code Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 03 set of problems for <adventofcode.com>.
-}
module Y2015.D03
  ( santaRun
  , roboRun
  ) where

import Data.Set  (Set)
import Data.Text (Text)
import Witch

import qualified Data.Set  as Set
import qualified Data.Text as T

type Point = (Int, Int)

direction :: Char -> Point
direction c
  | c == '^' = (0, 1)
  | c == 'v' = (0, -1)
  | c == '>' = (1, 0)
  | c == '<' = (-1, 0)
  | otherwise = (0, 0)

mapDirection :: Text -> [Point]
mapDirection = T.foldl' dir []
  where
    dir l c = direction c : l

start :: Set Point
start = Set.singleton (0, 0)

move :: Point -> Point -> Point
move (dx, dy) (x, y) = (x + dx, y + dy)

-- |Find number of deliverables for santa's route
santaRun
  :: Text -- ^ Route input
  -> Int  -- ^ Number of stops
santaRun = Set.size . deliver start . mapDirection

-- |Find number of deliverables for the robot's route
roboRun
  :: Text -- ^ Route input
  ->  Int -- ^ Number of stops
roboRun = Set.size . teamDelivery . tMap direction . divideWork . into @String
  where
    teamDelivery = uncurry (deliver . deliver start)

tMap :: (a -> b) -> ([a], [a]) -> ([b], [b])
tMap f (a1, a2) = (map f a1, map f a2)

divideWork :: String -> (String, String)
divideWork [] = ([], [])
divideWork [x] = ([x], [])
divideWork (x:y:zs) = (x : xp, y : yp)
  where
    (xp, yp) = divideWork zs

deliver :: Set Point -> [Point] -> Set Point
deliver = navigate (0, 0)

navigate :: Point -> Set Point -> [Point] -> Set Point
navigate _ history [] = history
navigate origin history (dir:plans) =
  let newPoint = move dir origin
      step = Set.insert newPoint history
  in navigate newPoint step plans
