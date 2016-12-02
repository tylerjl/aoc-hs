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

-- | Cardinal direction for movement
data Direction = North
               | East
               | South
               | West
               deriving (Enum)

-- | (x, y) coordinates
type Coordinates = (Int, Int)

-- | Find number of blocks traveled per list of instructions
blockDistance :: String -- ^ Input of directions
              -> Int -- ^ Number of blocks traveled
blockDistance = blocks
              . foldl' travel (0, 0)
              . mapRoute

-- | Find first block that is visited twice
visitedTwice :: String -- ^ Input of directions
             -> Maybe Int -- ^ Possible block that is visited twice
visitedTwice = navigate (0,0) Set.empty
             . mapRoute
    where navigate point set (path:xs)
              | Set.member point' set = Just $ blocks point'
              | otherwise = navigate point' (Set.insert point' set) xs
              where point' = travel point path
          navigate _ _ _ = Nothing

-- | Translate an input string into a list of movement `Direction`s
mapRoute :: String -- ^ Input string
         -> [Direction] -- ^ List of `Direction`s
mapRoute = toRoute North
         . map (toPath . filter  (not . (==) ','))
         . words
         where toRoute orientation ((lr, distance):xs) =
                   replicate distance orientation' ++ toRoute orientation' xs
                   where orientation' = turn lr orientation
               toRoute _ [] = []

-- | Calculate the number of blocks from the origin
blocks :: Coordinates -- ^ Current `Coordinates`
       -> Int -- ^ Number of blocks that `Coordinate` lies from the origin
blocks (x, y) = abs x + abs y

-- | Move along a given direction
travel :: Coordinates -- ^ Initial `Coordinates`
       -> Direction -- ^ `Direction` to move towards
       -> Coordinates -- ^ New `Coordinates`
travel (x, y) d = case d of
                    North -> (x, succ y)
                    East -> (succ x, y)
                    South -> (x, pred y)
                    West -> (pred x, y)

-- | Translate a movement instruction to an intermediate form
toPath :: String -- ^ Raw input instruction
       -> (Char, Int) -- ^ `Tuple` consisting of movement direction and
                      -- ^ distance
toPath (direction:steps) = (direction, read steps)
toPath [] = (' ', 0)

-- | Determine new direction after turning Left or Right
turn :: Char -- ^ 'L'eft or 'R'right
     -> Direction -- ^ Initial `Direction`
     -> Direction -- ^ New `Direction`
turn 'R' West = North
turn 'L' North = West
turn 'R' d = succ d
turn 'L' d = pred d
turn _ d = d
