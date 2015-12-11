#!/usr/bin/env runhaskell

module Y2015.D05
    ( clean
    , isNice
    , isNicer
    , thriceVoweled
    , twiceRow
) where

import Y2015.Util ((<&&>))

import Data.List     (group, isInfixOf)

isNicer :: String -> Bool
isNicer = repeatedPair <&&> repeatedBetween

repeatedPair :: String -> Bool
repeatedPair []                               = False
repeatedPair [x]                              = False
repeatedPair (x:y:zs) | [x, y] `isInfixOf` zs = True
                      | otherwise             = repeatedPair (y : zs)

repeatedBetween :: String -> Bool
repeatedBetween []                     = False
repeatedBetween [x]                    = False
repeatedBetween [x,y]                  = False
repeatedBetween (x:y:z:zs) | x == z    = True
                           | otherwise = repeatedBetween (y : z : zs)
isNice :: String -> Bool
isNice = clean <&&> thriceVoweled <&&> twiceRow

clean :: String -> Bool
clean = not . flip any forbiddenStrings . flip isInfixOf

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
       putStr "Part B - : "
       print (length $ filter isNicer $ lines input)
