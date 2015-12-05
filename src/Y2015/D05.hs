#!/usr/bin/env runhaskell

module Y2015.D05
    ( clean
    , isNice
    , thriceVoweled
    , twiceRow
) where

import Control.Monad (liftM2)
import Data.List     (group, isInfixOf)

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftM2 (&&)

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
