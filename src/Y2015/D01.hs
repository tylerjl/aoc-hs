#!/usr/bin/env runhaskell

module Y2015.D01
    ( level
    , basement )
where

import Data.List (foldl')

move :: Char -> Int
move c | c == '('  = 1
       | otherwise = -1

level :: String -> Int
level = foldl' (+) 0 . map move

basement :: String -> Maybe Int
basement = find 0 1 . map move
    where find current idx (move:moves)
               | current + move < 0 = Just idx
               | otherwise          = find (current + move) (idx + 1) moves
          find _ _ [] = Nothing

main :: IO ()
main = do
       input <- readFile "src/Y2015/D01_input"
       putStr "Part A: instructions arrive at floor "
       print (level input)
       putStr "Part B: basement arrival at position "
       print (basement input)
