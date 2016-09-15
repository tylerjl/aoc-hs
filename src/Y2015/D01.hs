{-|
Module:      Y2015.D01
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}

module Y2015.D01
    ( level
    , basement )
where

import Data.List (foldl')

move :: Char -> Int
move c | c == '('  = 1
       | c == ')'  = -1
       | otherwise = 0

-- |Find final level from list of elevator movements
level :: String -- ^ List of input open/close parens
      -> Int    -- ^ Final elevator level
level = foldl' (+) 0 . map move

-- |Find position that arrives at level 0
basement :: String    -- ^ List of input open/close parens
         -> Maybe Int -- ^ Possible position in string that arrives at zero
basement = find 0 1 . map move
    where find current idx (move:moves)
               | current + move < 0 = Just idx
               | otherwise          = find (current + move) (idx + 1) moves
          find _ _ [] = Nothing
