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

-- | A position on the keypad grid
type Position = (Int, Int)

-- | Wrapper to construct a `Matrix`
grid :: Int -- ^ Rows
     -> Int -- ^ Columns
     -> [a] -- ^ Elements
     -> Matrix.Matrix a -- ^ Resulting `Matrix`
grid = Matrix.fromList

-- | `Matrix` for part 1
grid1 :: Matrix.Matrix String
grid1 = grid 3 3 (map show ([1..] :: [Int]))

-- | `Matrix` for part 2
grid2 :: Matrix.Matrix String
grid2 = grid 5 5 $
  [ "", "",  "1",  "", ""
  , "", "2", "3", "4", ""
  ] ++ map show ([5..9] :: [Int]) ++
  [ "", "A", "B", "C", ""
  , "", "",  "D", "",  ""
  ]

bathroomCode :: Matrix.Matrix String -- ^ Grid to solve for
             -> Position -- ^ Starting `Position` (y, x)
             -> String -- ^ Input `String` of movement instructions
             -> String -- ^ Bathroom code
bathroomCode m origin = decode m "" origin
                      . lines

decode :: Matrix.Matrix String -- ^ Grid to solve for
       -> String -- ^ Solution key that's being built up
       -> Position -- ^ Current position
       -> [String] -- ^ List of movements for each code character
       -> String -- ^ Solution
decode _ key _ [] = key
decode m key position (moves:xs) =
  decode m (key ++ Matrix.getElem y x m) position' xs
  where position'@(x, y) = translate m position moves

translate :: Matrix.Matrix [a] -- ^ `Matrix` to move within bounds of
          -> Position -- ^ Starting `Position`
          -> String -- ^ List of directions to move
          -> Position -- ^ Final position after performing movements
translate _ position [] = position
translate m position (x:xs)
  | withinBounds position' m = translate m position' xs
  | otherwise = translate m position xs
  where position' = move position x

withinBounds :: Position -- ^ Coordinates of position to check
             -> Matrix.Matrix [a] -- ^ `Matrix` to test against
             -> Bool -- ^ Whether the given position lies within the matrix
withinBounds (x, y) m =
  case Matrix.safeGet y x m of
    Nothing -> False
    Just v -> not $ null v

move :: Position -- ^ Initial `Position`
     -> Char -- ^ Movement directive (any of "UDLR")
     -> Position -- ^ New `Position`
move (x, y) c =
  case c of
    'U' -> (x, y - 1)
    'R' -> (x + 1, y)
    'D' -> (x, y + 1)
    'L' -> (x - 1, y)
    _ -> (x, y)
