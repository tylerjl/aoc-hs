{-|
Module:      Y2016.D02
Description: Advent of Code Day 02 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}
module Y2016.D02
  ( bathroomCode
  ) where

import qualified Data.Matrix as Matrix

type Position = (Int, Int)

grid :: Matrix.Matrix Int
grid = Matrix.fromList 3 3 [1..]

bathroomCode :: String -> String
bathroomCode = decode "" (2, 2)
             . lines

decode :: String -> Position -> [String] -> String
decode key _ [] = key
decode key position (moves:xs) =
  decode (key ++ show (Matrix.getElem y x grid)) position' xs
  where position'@(x, y) = translate position moves

translate :: Position -> String -> Position
translate position [] = position
translate position (x:xs)
  | withinBounds position' grid = translate position' xs
  | otherwise = translate position xs
  where position' = move position x

withinBounds :: Position -> Matrix.Matrix a -> Bool
withinBounds (x, y) m =
  (x >= 1 && x <= Matrix.nrows m)
    && (y >= 1 && y <= Matrix.ncols m)

move :: Position -> Char -> Position
move (x, y) c =
  case c of
    'U' -> (x, y - 1)
    'R' -> (x + 1, y)
    'D' -> (x, y + 1)
    'L' -> (x - 1, y)
    _ -> (x, y)
