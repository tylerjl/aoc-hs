#!/usr/bin/env runhaskell

module Y2015.D10 (solve)  where

import Data.List (group, iterate)

solve = (!!) . iterate (concatMap walk . group)
    where walk s@(h:_) = show (length s) ++ [h]

main :: IO ()
main = do
    putStr "Part A - length is: "
    let stem = solve "1113122113"
    print $ length $ stem 40
    putStr "Part B - length is: "
    print $ length $ stem 50
