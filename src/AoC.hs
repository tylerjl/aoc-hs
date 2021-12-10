module AoC
  ( solve
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.String.Utils (rstrip)
import Data.Text (Text)
import Witch
import Y2015
import Y2016
import Y2018
import Y2021

import qualified Data.ByteString.Lazy as L

{-|Is there a nicer way to "dynamically" invoke these module functions based on
 year/day input? Maybe. But a big fat pattern match works, too. The small bit of
 glue between the outside world Main call and the solver functions.
-}
solve :: Int -> Int -> Char -> String -> String
solve 2015 1 'a' (level    -> solution) = show solution
solve 2015 1 'b' (basement -> solution) = show solution

solve 2015 2 'a' (fmap surfaceArea  . parsePresents -> Just solution) = show solution
solve 2015 2 'b' (fmap ribbonLength . parsePresents -> Just solution) = show solution
solve 2015 2  _  (parsePresents -> Nothing) = error "could not parse file"

solve 2015 3 'a' (santaRun . filter (/= '\n') -> solution) = show solution
solve 2015 3 'b' (roboRun  . filter (/= '\n') -> solution) = show solution

solve 2015 4 'a' (flip crack 5 . into @ByteString -> solution) = show solution
solve 2015 4 'b' (flip crack 6 . into @ByteString -> solution) = show solution

solve 2015 5 'a' (length . filter isNice  . lines -> solution) = show solution
solve 2015 5 'b' (length . filter isNicer . lines -> solution) = show solution

solve 2015 6 'a' (lightSimulation configureGridA . parseInstructions -> solution) = show solution
solve 2015 6 'b' (lightSimulation configureGridB . parseInstructions -> solution) = show solution

solve 2015 7 'a' (wire "a" . parseCircuits -> solution) = show solution
solve 2015 7 'b' (parseCircuits -> cs) = show $ aSolution (cs ++ [override (aSolution cs)])
  where aSolution = wire "a"

solve 2015 8 'a' (difference -> solution) = show solution
solve 2015 8 'b' (encoded    -> solution) = show solution

solve 2015 9 'a' (regularParse routeParser -> Right rs) = show $ shortestRoute rs
solve 2015 9 'b' (regularParse routeParser -> Right rs) = show $ longestRoute rs
solve 2015 9 _   (regularParse routeParser -> Left err) = error (show err)

solve 2015 10 'a' (length . flip lookSay 40 . rstrip -> solution) = show solution
solve 2015 10 'b' (length . flip lookSay 50 . rstrip -> solution) = show solution

solve 2015 11 'a' (rotate . rstrip -> solution) = show solution
solve 2015 11 'b' (rotate . rotate . rstrip -> solution) = show solution

solve 2015 12 'a' (jsonSum . into @L.ByteString -> solution) = show solution
solve 2015 12 'b' (jsonSumFixed . into @L.ByteString -> solution) = show solution

solve 2015 13 'a' (solveSeating -> solution) = show solution
solve 2015 13 'b' input = show $ solveSeating withMe
  where withMe = input ++
         "Yourself would gain 0 happiness units " ++
         "by sitting next to Yourself."

solve 2015 14 'a' (flip distanceRace 2503 -> solution) = show solution
solve 2015 14 'b' (flip leadingRace 2503 -> solution) = show solution

solve 2015 15 'a' (cookieScore  -> solution) = show solution
solve 2015 15 'b' (calorieScore -> solution) = show solution

solve 2015 16 'a' (findAunt     -> solution) = show solution
solve 2015 16 'b' (findRealAunt -> solution) = show solution

solve 2015 17 'a' (filledAmong 150 -> solution) = show solution
solve 2015 17 'b' (minFilledAmong 150 -> solution) = show solution

solve 2015 18 'a' (flip animateLights 100 -> solution) = show solution
solve 2015 18 'b' (flip animateStuckLights 100 -> solution) = show solution

solve 2015 19 'a' (distinctMols -> solution) = show solution
solve 2015 19 'b' (molSteps -> solution) = show solution

solve 2015 20 'a' _ = show (withMinPresents  36000000)
solve 2015 20 'b' _ = show (withMinPresents2 36000000)

solve 2015 21 'a' (cheapestVictory -> solution) = show solution
solve 2015 21 'b' (highestLoss     -> solution) = show solution

solve 2015 22 'a' (spellBattle False -> solution) = show solution
solve 2015 22 'b' (spellBattle True  -> solution) = show solution

solve 2015 23 'a' (exInstructions -> solution) = show solution
solve 2015 23 'b' (exInstructions2 -> solution) = show solution

solve 2015 24 'a' (idealEntanglement 3 -> solution) = show solution
solve 2015 24 'b' (idealEntanglement 4 -> solution) = show solution

solve 2015 25  _ (manualCodeFrom -> solution) = show solution

solve 2016 1 'a' (blockDistance -> solution) = show solution
solve 2016 1 'b' (visitedTwice  -> solution) = show solution

solve 2016 2 'a' (bathroomCode grid1 (2,2) -> solution) = show solution
solve 2016 2 'b' (bathroomCode grid2 (1,3) -> solution) = show solution

solve 2018 1 'a' (frequency -> solution) = show solution
solve 2018 1 'b' (twiceFrequency -> solution) = show solution

solve 2018 2 'a' (checksum -> solution) = show solution
solve 2018 2 'b' (boxID -> solution) = show solution

solve 2018 3 'a' (overlappedInches -> solution) = show solution
solve 2018 3 'b' (intactInches -> solution) = show solution

solve 2018 4 'a' (laziestGuard -> solution) = show solution
solve 2018 4 'b' (laziestMinute -> solution) = show solution

solve 2018 5 'a' (react . rstrip -> solution) = show solution
solve 2018 5 'b' (reactBest . rstrip -> solution) = show solution

solve 2021 1 'a' (part1A . into @Text -> solution) = show solution
solve 2021 1 'b' (part1B . into @Text -> solution) = show solution

solve 2021 2 'a' (part2A . into @Text -> solution) = show solution
solve 2021 2 'b' (part2B . into @Text -> solution) = show solution

solve 2021 3 'a' (part3A . into @Text -> solution) = show solution
solve 2021 3 'b' (part3B . into @Text -> solution) = show solution

solve 2021 4 'a' (part4A . into @Text -> solution) = show solution
solve 2021 4 'b' (part4B . into @Text -> solution) = show solution

solve 2021 5 'a' (part5A . into @Text -> solution) = show solution
solve 2021 5 'b' (part5B . into @Text -> solution) = show solution

solve 2021 6 'a' (part6AMV . into @Text -> solution) = show solution
solve 2021 6 'b' (part6BMV . into @Text -> solution) = show solution

solve 2021 7 'a' (part7A . into @Text -> solution) = show solution
solve 2021 7 'b' (part7B . into @Text -> solution) = show solution

solve 2021 8 'a' (part8A . into @Text -> solution) = show solution
solve 2021 8 'b' (part8B . into @Text -> solution) = show solution

solve 2021 9 'a' (part9A . into @Text -> solution) = show solution
solve 2021 9 'b' (part9B . into @Text -> solution) = show solution

solve y d p _ = error $
  "I can't handle year " <> show y <> " day " <> show d <> " part " <> show p
