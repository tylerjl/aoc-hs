#!/usr/bin/env runhaskell

module Y2015.D05
    ( thriceVoweled
    , clean
    , twiceRow
    , isNice
) where

import Data.List (group, isInfixOf)

isNice :: String -> Bool
isNice s = clean s && thriceVoweled s && twiceRow s

clean :: String -> Bool
clean s = not $ any (\x -> x `isInfixOf` s) forbiddenStrings

forbiddenStrings :: [String]
forbiddenStrings = ["ab", "cd", "pq", "xy"]

twiceRow :: String -> Bool
twiceRow = any ((>1) . length) . group

thriceVoweled :: String -> Bool
thriceVoweled = (>2) . length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

main :: IO ()
main = do
       input <- readFile "src/Y2015/D05_input"
       putStr "Part A - : "
       print (length $ filter isNice $ lines input)
