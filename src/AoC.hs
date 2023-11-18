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
import qualified Data.Text            as T

{-|Is there a nicer way to "dynamically" invoke these module functions based on
 year/day input? Maybe. But a big fat pattern match works, too. The small bit of
 glue between the outside world Main call and the solver functions.
-}
solve :: Int -> Int -> Char -> Text -> String
solve 2015 1 'a' (level -> solution) = show solution
solve 2015 1 'b' (basement -> solution) = show solution

solve 2015 2 'a' (fmap surfaceArea  . parsePresents -> Just solution) = show solution
solve 2015 2 'b' (fmap ribbonLength . parsePresents -> Just solution) = show solution
solve 2015 2  _  (parsePresents . into @Text -> Nothing) = error "could not parse file"

solve 2015 3 'a' (santaRun . T.filter (/= '\n') -> solution) = show solution
solve 2015 3 'b' (roboRun  . T.filter (/= '\n') -> solution) = show solution

solve 2015 4 'a' (flip crack 5 . into . into @(UTF_8 ByteString) -> solution) = show solution
solve 2015 4 'b' (flip crack 6 . into . into @(UTF_8 ByteString) -> solution) = show solution

solve 2015 5 'a' (length . filter isNice  . lines . into @String -> solution) = show solution
solve 2015 5 'b' (length . filter isNicer . lines . into @String -> solution) = show solution

solve 2015 6 'a' (lightSimulation configureGridA . parseInstructions . into @String -> solution) = show solution
solve 2015 6 'b' (lightSimulation configureGridB . parseInstructions . into @String -> solution) = show solution

solve 2015 7 'a' (wire "a" . parseCircuits . into @String -> solution) = show solution
solve 2015 7 'b' (parseCircuits . into @String -> cs) = show $ aSolution (cs ++ [override (aSolution cs)])
  where aSolution = wire "a"

solve 2015 8 'a' (difference . into @String -> solution) = show solution
solve 2015 8 'b' (encoded    . into @String -> solution) = show solution

solve 2015 9 'a' (regularParse routeParser . into @String -> Right rs) = show $ shortestRoute rs
solve 2015 9 'b' (regularParse routeParser . into @String -> Right rs) = show $ longestRoute rs
solve 2015 9 _   (regularParse routeParser . into @String -> Left err) = error (show err)

solve 2015 10 'a' (length . flip lookSay 40 . rstrip . into @String -> solution) = show solution
solve 2015 10 'b' (length . flip lookSay 50 . rstrip . into @String -> solution) = show solution

solve 2015 11 'a' (rotate . rstrip . into @String -> solution) = show solution
solve 2015 11 'b' (rotate . rotate . rstrip . into @String -> solution) = show solution

solve 2015 12 'a' (jsonSum . into . into @(UTF_8 L.ByteString) -> solution) = show solution
solve 2015 12 'b' (jsonSumFixed . into . into @(UTF_8 L.ByteString) -> solution) = show solution

solve 2015 13 'a' (solveSeating . into @String -> solution) = show solution
solve 2015 13 'b' (into @String -> input) = show $ solveSeating withMe
  where withMe = input ++
         "Yourself would gain 0 happiness units " ++
         "by sitting next to Yourself."

solve 2015 14 'a' (flip distanceRace 2503 . into @String -> solution) = show solution
solve 2015 14 'b' (flip leadingRace 2503 . into @String -> solution) = show solution

solve 2015 15 'a' (cookieScore  . into @String -> solution) = show solution
solve 2015 15 'b' (calorieScore . into @String -> solution) = show solution

solve 2015 16 'a' (findAunt     . into @String -> solution) = show solution
solve 2015 16 'b' (findRealAunt . into @String -> solution) = show solution

solve 2015 17 'a' (filledAmong 150 . into @String -> solution) = show solution
solve 2015 17 'b' (minFilledAmong 150 . into @String -> solution) = show solution

solve 2015 18 'a' (flip animateLights 100 . into @String -> solution) = show solution
solve 2015 18 'b' (flip animateStuckLights 100 . into @String -> solution) = show solution

solve 2015 19 'a' (distinctMols . into @String -> solution) = show solution
solve 2015 19 'b' (molSteps . into @String -> solution) = show solution

solve 2015 20 'a' _ = show (withMinPresents  36000000)
solve 2015 20 'b' _ = show (withMinPresents2 36000000)

solve 2015 21 'a' (cheapestVictory . into @String -> solution) = show solution
solve 2015 21 'b' (highestLoss     . into @String -> solution) = show solution

solve 2015 22 'a' (spellBattle False . into @String -> solution) = show solution
solve 2015 22 'b' (spellBattle True  . into @String -> solution) = show solution

solve 2015 23 'a' (exInstructions . into @String -> solution) = show solution
solve 2015 23 'b' (exInstructions2 . into @String -> solution) = show solution

solve 2015 24 'a' (idealEntanglement 3 . into @String -> solution) = show solution
solve 2015 24 'b' (idealEntanglement 4 . into @String -> solution) = show solution

solve 2015 25  _ (manualCodeFrom . into @String -> solution) = show solution

solve 2016 1 'a' (blockDistance . into @String -> solution) = show solution
solve 2016 1 'b' (visitedTwice  . into @String -> solution) = show solution

solve 2016 2 'a' (bathroomCode grid1 (2,2) . into @String -> solution) = show solution
solve 2016 2 'b' (bathroomCode grid2 (1,3) . into @String -> solution) = show solution

solve 2018 1 'a' (frequency . into @String -> solution) = show solution
solve 2018 1 'b' (twiceFrequency . into @String -> solution) = show solution

solve 2018 2 'a' (checksum . into @String -> solution) = show solution
solve 2018 2 'b' (boxID . into @String -> solution) = show solution

solve 2018 3 'a' (overlappedInches . into @String -> solution) = show solution
solve 2018 3 'b' (intactInches . into @String -> solution) = show solution

solve 2018 4 'a' (laziestGuard . into @String -> solution) = show solution
solve 2018 4 'b' (laziestMinute . into @String -> solution) = show solution

solve 2018 5 'a' (react . rstrip . into @String -> solution) = show solution
solve 2018 5 'b' (reactBest . rstrip . into @String -> solution) = show solution

solve 2021 1 'a' (part1A -> solution) = show solution
solve 2021 1 'b' (part1B -> solution) = show solution

solve 2021 2 'a' (part2A -> solution) = show solution
solve 2021 2 'b' (part2B -> solution) = show solution

solve 2021 3 'a' (part3A -> solution) = show solution
solve 2021 3 'b' (part3B -> solution) = show solution

solve 2021 4 'a' (part4A -> solution) = show solution
solve 2021 4 'b' (part4B -> solution) = show solution

solve 2021 5 'a' (part5A -> solution) = show solution
solve 2021 5 'b' (part5B -> solution) = show solution

solve 2021 6 'a' (part6AMV -> solution) = show solution
solve 2021 6 'b' (part6BMV -> solution) = show solution

solve 2021 7 'a' (part7A -> solution) = show solution
solve 2021 7 'b' (part7B -> solution) = show solution

solve 2021 8 'a' (part8A -> solution) = show solution
solve 2021 8 'b' (part8B -> solution) = show solution

solve 2021 9 'a' (part9A -> solution) = show solution
solve 2021 9 'b' (part9B -> solution) = show solution

solve 2021 10 'a' (part10A -> solution) = show solution
solve 2021 10 'b' (part10B -> solution) = show solution

solve 2021 11 'a' (part11A -> solution) = show solution
solve 2021 11 'b' (part11B -> solution) = show solution

solve 2021 12 'a' (part12A -> solution) = show solution
solve 2021 12 'b' (part12B -> solution) = show solution

solve 2021 13 'a' (part13A -> solution) = show solution
solve 2021 13 'b' (part13B -> solution) = show solution

solve 2021 14 'a' (part14A -> solution) = show solution
solve 2021 14 'b' (part14B -> solution) = show solution

solve 2021 15 'a' (part15A -> solution) = show solution
solve 2021 15 'b' (part15B -> solution) = show solution

solve 2021 16 'a' (part16A -> solution) = show solution
solve 2021 16 'b' (part16B -> solution) = show solution

solve 2021 17 'a' (part17A -> solution) = show solution
solve 2021 17 'b' (part17B -> solution) = show solution

solve 2021 18 'a' (part18A -> solution) = show solution
solve 2021 18 'b' (part18B -> solution) = show solution

solve 2021 19 'a' (part19A -> solution) = show solution
solve 2021 19 'b' (part19B -> solution) = show solution

solve 2021 20 'a' (part20A -> solution) = show solution
solve 2021 20 'b' (part20B -> solution) = show solution

solve y d p _ = error $
  "I can't handle year " <> show y <> " day " <> show d <> " part " <> show p
