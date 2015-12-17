module Y2015.D17 (filledAmong) where

import Data.List (subsequences)

filledAmong :: Int -> String -> Int
filledAmong total s = length [x | x <- subsequences containers, sum x == total]
    where containers = map read $ lines s
