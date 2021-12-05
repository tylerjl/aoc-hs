{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module:      Main
Description: Advent of Code solution driver.
License:     MIT
Maintainer:  @tylerjl

Entrypoint for the CLI interface to AoC solutions in Haskell.
-}
module Main where

import Data.String.Utils (rstrip)
import Data.Text         (Text)
import Options.Generic   (unwrapRecord)

import qualified Data.ByteString.Lazy as L

import Options
import Y2015
import Y2016
import Y2018
import Y2021
import Data.ByteString.Char8 (ByteString)
import Witch

-- |CLI boilerplate
usage :: Text
usage = "Advent of Code solutions in Haskell"

{-|For use of the solutions here outside of a repl if you'd rather build and
 invoke the solution functions.
-}
main :: IO ()
main = do
  (Options year day part path) <- unwrapRecord usage
  readFile path >>= putStrLn . solver year day part

{-|Is there a nicer way to "dynamically" invoke these module functions based on
 year/day input? Maybe. But a big fat pattern match works, too. The small bit of
 glue between the outside world Main call and the solver functions.
-}
solver :: Int -> Int -> Char -> String -> String
solver 2015 1 'a' (level    -> solution) = show solution
solver 2015 1 'b' (basement -> solution) = show solution

solver 2015 2 'a' (fmap surfaceArea  . parsePresents -> Just solution) = show solution
solver 2015 2 'b' (fmap ribbonLength . parsePresents -> Just solution) = show solution
solver 2015 2  _  (parsePresents -> Nothing) = error "could not parse file"

solver 2015 3 'a' (santaRun . filter (/= '\n') -> solution) = show solution
solver 2015 3 'b' (roboRun  . filter (/= '\n') -> solution) = show solution

solver 2015 4 'a' (flip crack 5 . into @ByteString -> solution) = show solution
solver 2015 4 'b' (flip crack 6 . into @ByteString -> solution) = show solution

solver 2015 5 'a' (length . filter isNice  . lines -> solution) = show solution
solver 2015 5 'b' (length . filter isNicer . lines -> solution) = show solution

solver 2015 6 'a' (lightSimulation configureGridA . parseInstructions -> solution) = show solution
solver 2015 6 'b' (lightSimulation configureGridB . parseInstructions -> solution) = show solution

solver 2015 7 'a' (wire "a" . parseCircuits -> solution) = show solution
solver 2015 7 'b' (parseCircuits -> cs) = show $ aSolution (cs ++ [override (aSolution cs)])
  where aSolution = wire "a"

solver 2015 8 'a' (difference -> solution) = show solution
solver 2015 8 'b' (encoded    -> solution) = show solution

solver 2015 9 'a' (regularParse routeParser -> Right rs) = show $ shortestRoute rs
solver 2015 9 'b' (regularParse routeParser -> Right rs) = show $ longestRoute rs
solver 2015 9 _   (regularParse routeParser -> Left err) = error (show err)

solver 2015 10 'a' (length . flip lookSay 40 . rstrip -> solution) = show solution
solver 2015 10 'b' (length . flip lookSay 50 . rstrip -> solution) = show solution

solver 2015 11 'a' (rotate . rstrip -> solution) = show solution
solver 2015 11 'b' (rotate . rotate . rstrip -> solution) = show solution

solver 2015 12 'a' (jsonSum . into @L.ByteString -> solution) = show solution
solver 2015 12 'b' (jsonSumFixed . into @L.ByteString -> solution) = show solution

solver 2015 13 'a' (solveSeating -> solution) = show solution
solver 2015 13 'b' input = show $ solveSeating withMe
  where withMe = input ++
          "Yourself would gain 0 happiness units " ++
          "by sitting next to Yourself."

solver 2015 14 'a' (flip distanceRace 2503 -> solution) = show solution
solver 2015 14 'b' (flip leadingRace 2503 -> solution) = show solution

solver 2015 15 'a' (cookieScore  -> solution) = show solution
solver 2015 15 'b' (calorieScore -> solution) = show solution

solver 2015 16 'a' (findAunt     -> solution) = show solution
solver 2015 16 'b' (findRealAunt -> solution) = show solution

solver 2015 17 'a' (filledAmong 150 -> solution) = show solution
solver 2015 17 'b' (minFilledAmong 150 -> solution) = show solution

solver 2015 18 'a' (flip animateLights 100 -> solution) = show solution
solver 2015 18 'b' (flip animateStuckLights 100 -> solution) = show solution

solver 2015 19 'a' (distinctMols -> solution) = show solution
solver 2015 19 'b' (molSteps -> solution) = show solution

solver 2015 20 'a' _ = show (withMinPresents  36000000)
solver 2015 20 'b' _ = show (withMinPresents2 36000000)

solver 2015 21 'a' (cheapestVictory -> solution) = show solution
solver 2015 21 'b' (highestLoss     -> solution) = show solution

solver 2015 22 'a' (spellBattle False -> solution) = show solution
solver 2015 22 'b' (spellBattle True  -> solution) = show solution

solver 2015 23 'a' (exInstructions -> solution) = show solution
solver 2015 23 'b' (exInstructions2 -> solution) = show solution

solver 2015 24 'a' (idealEntanglement 3 -> solution) = show solution
solver 2015 24 'b' (idealEntanglement 4 -> solution) = show solution

solver 2015 25  _ (manualCodeFrom -> solution) = show solution

solver 2016 1 'a' (blockDistance -> solution) = show solution
solver 2016 1 'b' (visitedTwice  -> solution) = show solution

solver 2016 2 'a' (bathroomCode grid1 (2,2) -> solution) = show solution
solver 2016 2 'b' (bathroomCode grid2 (1,3) -> solution) = show solution

solver 2018 1 'a' (frequency -> solution) = show solution
solver 2018 1 'b' (twiceFrequency -> solution) = show solution

solver 2018 2 'a' (checksum -> solution) = show solution
solver 2018 2 'b' (boxID -> solution) = show solution

solver 2018 3 'a' (overlappedInches -> solution) = show solution
solver 2018 3 'b' (intactInches -> solution) = show solution

solver 2018 4 'a' (laziestGuard -> solution) = show solution
solver 2018 4 'b' (laziestMinute -> solution) = show solution

solver 2018 5 'a' (react . rstrip -> solution) = show solution
solver 2018 5 'b' (reactBest . rstrip -> solution) = show solution

solver 2021 1 'a' (partA . into @Text -> solution) = show solution
solver 2021 1 'b' (partB . into @Text -> solution) = show solution

solver 2021 2 'a' (part2A . into @Text -> solution) = show solution
solver 2021 2 'b' (part2B . into @Text -> solution) = show solution

solver 2021 3 'a' (part3A . into @Text -> solution) = show solution
solver 2021 3 'b' (part3B . into @Text -> solution) = show solution

solver 2021 4 'a' (part4A . into @Text -> solution) = show solution
solver 2021 4 'b' (part4B . into @Text -> solution) = show solution

solver 2021 5 'a' (part5A . into @Text -> solution) = show solution
solver 2021 5 'b' (part5B . into @Text -> solution) = show solution

solver y d p _ = error $
  "I can't handle year " <> show y <> " day " <> show d <> " part " <> show p
