module Y2015.D17 (filledAmong, minFilledAmong) where

import Data.Function (on)
import Data.List (subsequences, minimumBy)

filledAmong :: Int -> String -> Int
filledAmong t = containersBy length t . c
    where c = map read . lines

minFilledAmong :: Int -> String -> Int
minFilledAmong t s =  containersBy (length . filter ((==) minFilled . length)) t c
    where minFilled = containersBy (length . minimumBy (compare `on` length)) t c
          c = map read $ lines s

containersBy :: ([[Int]] -> a) -> Int -> [Int] -> a
containersBy f t s = f [x | x <- subsequences s, sum x == t]
