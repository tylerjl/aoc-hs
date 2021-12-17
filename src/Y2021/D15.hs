{-# LANGUAGE TypeFamilies #-}
{-|
Module:      Y2021.D15
Description: Advent of Code 2021 Day 15 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 15 set of problems for <adventofcode.com>.
-}
module Y2021.D15
  ( parse15
  , part15A
  , part15B
  ) where

import Control.Arrow
import Data.Attoparsec.Text hiding  (take)
import Data.Either.Utils            (fromRight)
import Data.Foldable
import Data.List.Extra              (transpose)
import Data.Map.Strict              (Map)
import Data.Text                    (Text)
import Math.Geometry.Grid hiding    (distance)
import Math.Geometry.Grid.Square
import Math.Geometry.GridMap hiding (foldl', map, filter)
import Math.Geometry.GridMap.Lazy

import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import qualified Math.Geometry.GridMap as GM

type Cavern = LGridMap RectSquareGrid Int
data Distance a
  = Infinite
  | Distance a
  deriving (Eq, Show)

instance Ord a => Ord (Distance a) where
  Infinite     `compare` Infinite     = EQ
  (Distance _) `compare` Infinite     = LT
  (Distance a) `compare` (Distance b) = a `compare` b
  Infinite     `compare` (Distance _) = GT

type Paths = Map (Int, Int) (Distance Int)

-- |Solution to part A
part15A :: Text -> Int
part15A (parse15 -> grid) =
  case flip (M.!) (endPosition grid) $ cheapestPath grid of
    Distance d -> d
    Infinite -> error "can't reach goal"

cheapestPath :: Cavern -> Paths
cheapestPath grid = go (H.singleton (H.Entry (Distance 0) (0, 0))) nodes
  where
    go (H.viewMin -> Nothing) paths = paths
    go (H.viewMin -> Just (H.Entry dist point, q)) paths
      = go q' paths'
      where
        (q', paths') = foldl' measure (q, paths)
            [(n, grid ! n) | n <- neighbours grid point]
        measure (heap, costs) (neigh, cost)
          | alt < toNeigh = ( H.insert (H.Entry alt neigh) heap
                            , M.insert neigh alt costs
                            )
          | otherwise = (heap, costs)
          where
            alt = dist `addDist` Distance cost
            toNeigh = costs M.! neigh
    allNodes = map fst $ GM.toList grid
    nodes = M.fromList $ zip allNodes (repeat Infinite)

endPosition :: Cavern -> (Int, Int)
endPosition = (pred *** pred) . size

addDist :: Num a => Distance a -> Distance a -> Distance a
addDist (Distance a) (Distance b) = Distance (a + b)
addDist Infinite (Distance b) = Distance b
addDist (Distance a) Infinite = Distance a
addDist Infinite Infinite = Infinite

-- |Solution to part B
part15B :: Text -> Int
part15B (expandBy 5 . parse15 -> grid) =
  case flip (M.!) (endPosition grid) $ cheapestPath grid of
    Distance d -> d
    Infinite -> error "can't reach goal"

expandBy :: Int -> Cavern -> Cavern
expandBy n g
  = GM.mapWithKey populate $ lazyGridMap (rectSquareGrid rows' cols') (repeat 0 :: [Int])
  where
    (rows, cols) = size g
    (rows', cols') = (rows * n, cols * n)
    populate (x, y) _ = iterate elevate (g ! (x `mod` cols, y `mod` rows)) !! (extX + extY)
      where (extX, extY) = (x `div` cols, y `div` rows)

elevate :: Integral a => a -> a
elevate n = max 1 ((n + 1) `mod` 10)

-- |Parse.
parse15 :: Text -> Cavern
parse15 = fromRight . parseOnly (grid <$> parser)
  where
    grid [] = error "empty input"
    grid rows@(row:_) =
      lazyGridMap (rectSquareGrid (length row) (length rows)) (concat $ transpose rows)
    parser = line `sepBy1` endOfLine <* atEnd
    line = many1 (read . (: []) <$> digit)
