{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.String.Utils (rstrip)
import Options.Generic
import Y2015
import Y2016
import Y2018

type ArgYear = Int
type ArgDay  = Int
type ArgPath = String
data Options w =
  Options (w ::: ArgYear <?> "Year")
          (w ::: ArgDay <?> "Day")
          (w ::: ArgPath <?> "Input file path")
  deriving Generic
instance ParseRecord (Options Wrapped)

usage :: Text
usage = "Advent of Code solutions in Haskell"

main :: IO ()
main = do
  (Options year day path) <- unwrapRecord usage
  run year day path

run :: Int -> Int -> String -> IO ()
run 2015 1 file = do
  contents <- readFile file
  print (level contents, basement contents)
run 2015 2 file = do
  contents <- readFile file
  case parsePresents contents of
    Nothing -> putStrLn $ "Could not parse" ++ file
    Just ps -> print (surfaceArea ps, ribbonLength ps)
run 2015 3 file = do
  c <- readFile file
  let contents = filter (/= '\n') c
  print (santaRun contents, roboRun contents)
run 2015 4 file = do
  contents <- B.readFile file
  print (crack contents 5, crack contents 6)
run 2015 5 file =
  let check f = length . filter f . lines
  in do contents <- readFile file
        print (check isNice contents, check isNicer contents)
run 2015 6 file = do
  contents <- readFile file
  case parseInstructions contents of
    Left e -> putStrLn $ "Could not parse: " ++ show e
    Right insts -> do
      a <- lightSimulation configureGridA insts
      b <- lightSimulation configureGridB insts
      print (a, b)
run 2015 7 file = do
  contents <- readFile file
  case parseCircuits contents of
    Left e -> putStrLn $ "Could not parse: " ++ show e
    Right insts ->
      let a = wire "a" insts
      in print (a, wire "a" (insts ++ [override a]))
run 2015 8 file = do
  contents <- readFile file
  print (difference contents, encoded contents)
run 2015 9 file = do
  contents <- readFile file
  case regularParse routeParser contents of
    Left e -> putStrLn $ "Could not parse: " ++ show e
    Right rs -> print (shortestRoute rs, longestRoute rs)
run 2015 10 file = do
  contents <- readFile file
  let stem = lookSay $ rstrip contents
  print (length $ stem 40, length $ stem 50)
run 2015 11 file = do
  contents <- readFile file
  let pw = rotate $ rstrip contents
  print (pw, rotate pw)
run 2015 12 file = do
  contents <- L.readFile file
  print (jsonSum contents, jsonSumFixed contents)
run 2015 13 file = do
  contents <- readFile file
  let withMe =
        contents ++
        "Yourself would gain 0 happiness units " ++
        "by sitting next to Yourself."
  print (solveSeating contents, solveSeating withMe)
run 2015 14 file = do
  contents <- readFile file
  print (distanceRace contents 2503, leadingRace contents 2503)
run 2015 15 file = do
  contents <- readFile file
  print (cookieScore contents, calorieScore contents)
run 2015 16 file = do
  contents <- readFile file
  print (findAunt contents, findRealAunt contents)
run 2015 17 file = do
  contents <- readFile file
  print (150 `filledAmong` contents, 150 `minFilledAmong` contents)
run 2015 18 file = do
  contents <- readFile file
  print (animateLights contents 100, animateStuckLights contents 100)
run 2015 19 file = do
  contents <- readFile file
  print (distinctMols contents, molSteps contents)
run 2015 20 _ = print (withMinPresents 36000000, withMinPresents2 36000000)
run 2015 21 file = do
  contents <- readFile file
  print (cheapestVictory contents, highestLoss contents)
run 2015 22 file = do
  contents <- readFile file
  print (spellBattle False contents, spellBattle True contents)
run 2015 23 file = do
  contents <- readFile file
  print (exInstructions contents, exInstructions2 contents)
run 2015 24 file = do
  contents <- readFile file
  print (idealEntanglement 3 contents, idealEntanglement 4 contents)
run 2015 25 file = do
  contents <- readFile file
  print (manualCodeFrom contents)

run 2016 1 file = do
  contents <- readFile file
  print (blockDistance contents, visitedTwice contents)
run 2016 2 file = do
  contents <- readFile file
  print ( bathroomCode grid1 (2,2) contents
        , bathroomCode grid2 (1,3) contents
        )

run 2018 1 file = do
  contents <- readFile file
  case frequency contents of
    Nothing -> putStrLn $ "Could not parse" ++ file
    Just i -> print i
  case twiceFrequency contents of
    Nothing -> putStrLn $ "Could not parse" ++ file
    Just i -> print i

run 2018 2 file = do
  contents <- readFile file
  print $ checksum contents
  case boxID contents of
    Nothing -> print ("Couldn't find matching box." :: String)
    Just s  -> print s

run 2018 3 file = do
  contents <- readFile file
  case overlappedInches contents of
    Left e -> print e
    Right s -> print s
  print $ intactInches contents

run 2018 4 file = do
  contents <- readFile file
  case laziestGuard contents of
    Left e -> print e
    Right s -> print s
  case laziestMinute contents of
    Left e -> print e
    Right s -> print s

run 2018 5 file = do
  contents <- readFile file
  print $ react $ rstrip contents
  print $ reactBest $ rstrip contents

run _ _ _ = putStrLn "Not implemented yet."
