{-|
Module:      Y2016.D02
Description: Advent of Code Day 02 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}
module Y2016.D02
  ( bathroomCode
  , grid1
  , grid2
  ) where

import qualified Data.Matrix as Matrix

type Position = (Int, Int)

grid :: Int -> Int -> [a] -> Matrix.Matrix a
grid = Matrix.fromList

grid1 :: Matrix.Matrix String
grid1 = grid 3 3 (map show ([1..] :: [Int]))

grid2 :: Matrix.Matrix String
grid2 = grid 5 5 $
  [ "", "",  "1",  "", ""
  , "", "2", "3", "4", ""
  ] ++ map show ([5..9] :: [Int]) ++
  [ "", "A", "B", "C", ""
  , "", "",  "D", "",  ""
  ]

bathroomCode :: Matrix.Matrix String -> Position -> String -> String
bathroomCode m origin = decode m "" origin
                      . lines

decode :: Matrix.Matrix String -> String -> Position -> [String] -> String
decode _ key _ [] = key
decode m key position (moves:xs) =
  decode m (key ++ Matrix.getElem y x m) position' xs
  where position'@(x, y) = translate m position moves

translate :: Matrix.Matrix [a] -> Position -> String -> Position
translate _ position [] = position
translate m position (x:xs)
  | withinBounds position' m = translate m position' xs
  | otherwise = translate m position xs
  where position' = move position x

withinBounds :: Position -> Matrix.Matrix [a] -> Bool
withinBounds (x, y) m =
  case Matrix.safeGet y x m of
    Nothing -> False
    Just v -> not $ null v

move :: Position -> Char -> Position
move (x, y) c =
  case c of
    'U' -> (x, y - 1)
    'R' -> (x + 1, y)
    'D' -> (x, y + 1)
    'L' -> (x - 1, y)
    _ -> (x, y)
