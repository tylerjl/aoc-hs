{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Foldl as Fold
import           Data.List        (genericLength)
import           Data.Maybe       (catMaybes)
import           Data.Text        (unpack)
import           System.Exit      (exitFailure, exitSuccess)
import           Text.Regex       (matchRegex, mkRegex)
import           Turtle           (inprocWithErr, fold)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    text <- fold
        (fmap (\(Left o) -> o) (inprocWithErr "stack" ["haddock"] ""))
        Fold.list
    let output = map unpack text
        avg = average $ match output
    putStrLn $ "\nAverage documentation coverage: " ++ show avg
    if avg >= expected
        then exitSuccess
        else exitFailure

match :: [String] -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex p)
  where p = mkRegex "^ *([0-9]*)% "
