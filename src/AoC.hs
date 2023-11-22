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
solve :: Int -> Int -> Char -> [String] -> Text -> String
solve 2015 1 'a' _ (level -> solution) = show solution
solve 2015 1 'b' _ (basement -> solution) = show solution

solve 2015 2 'a' _ (fmap surfaceArea  . parsePresents -> Just solution) = show solution
solve 2015 2 'b' _ (fmap ribbonLength . parsePresents -> Just solution) = show solution
solve 2015 2  _  _ (parsePresents . into @Text -> Nothing) = error "could not parse file"

solve 2015 3 'a' _ (santaRun . T.filter (/= '\n') -> solution) = show solution
solve 2015 3 'b' _ (roboRun  . T.filter (/= '\n') -> solution) = show solution

solve 2015 4 'a' _ (flip crack 5 . into . into @(UTF_8 ByteString) -> solution) = show solution
solve 2015 4 'b' _ (flip crack 6 . into . into @(UTF_8 ByteString) -> solution) = show solution

solve 2015 5 'a' _ (length . filter isNice  . lines . into @String -> solution) = show solution
solve 2015 5 'b' _ (length . filter isNicer . lines . into @String -> solution) = show solution

solve 2015 6 'a' _ (lightSimulation configureGridA . parseInstructions . into @String -> solution) = show solution
solve 2015 6 'b' _ (lightSimulation configureGridB . parseInstructions . into @String -> solution) = show solution

solve 2015 7 'a' _ (wire "a" . parseCircuits . into @String -> solution) = show solution
solve 2015 7 'b' _ (parseCircuits . into @String -> cs) = show $ aSolution (cs ++ [override (aSolution cs)])
  where aSolution = wire "a"

solve 2015 8 'a' _ (difference . into @String -> solution) = show solution
solve 2015 8 'b' _ (encoded    . into @String -> solution) = show solution

solve 2015 9 'a' _ (regularParse routeParser . into @String -> Right rs) = show $ shortestRoute rs
solve 2015 9 'b' _ (regularParse routeParser . into @String -> Right rs) = show $ longestRoute rs
solve 2015 9 _   _ (regularParse routeParser . into @String -> Left err) = error (show err)

solve 2015 10 'a' _ (length . flip lookSay 40 . rstrip . into @String -> solution) = show solution
solve 2015 10 'b' _ (length . flip lookSay 50 . rstrip . into @String -> solution) = show solution

solve 2015 11 'a' _ (rotate . rstrip . into @String -> solution) = show solution
solve 2015 11 'b' _ (rotate . rotate . rstrip . into @String -> solution) = show solution

solve 2015 12 'a' _ (jsonSum . into . into @(UTF_8 L.ByteString) -> solution) = show solution
solve 2015 12 'b' _ (jsonSumFixed . into . into @(UTF_8 L.ByteString) -> solution) = show solution

solve 2015 13 'a' _ (solveSeating . into @String -> solution) = show solution
solve 2015 13 'b' _ (into @String -> input) = show $ solveSeating withMe
  where withMe = input ++
         "Yourself would gain 0 happiness units " ++
         "by sitting next to Yourself."

solve 2015 14 'a' _ (flip distanceRace 2503 . into @String -> solution) = show solution
solve 2015 14 'b' _ (flip leadingRace 2503 . into @String -> solution) = show solution

solve 2015 15 'a' _ (cookieScore  . into @String -> solution) = show solution
solve 2015 15 'b' _ (calorieScore . into @String -> solution) = show solution

solve 2015 16 'a' _ (findAunt     . into @String -> solution) = show solution
solve 2015 16 'b' _ (findRealAunt . into @String -> solution) = show solution

solve 2015 17 'a' _ (filledAmong 150 . into @String -> solution) = show solution
solve 2015 17 'b' _ (minFilledAmong 150 . into @String -> solution) = show solution

solve 2015 18 'a' _ (flip animateLights 100 . into @String -> solution) = show solution
solve 2015 18 'b' _ (flip animateStuckLights 100 . into @String -> solution) = show solution

solve 2015 19 'a' _ (distinctMols . into @String -> solution) = show solution
solve 2015 19 'b' _ (molSteps . into @String -> solution) = show solution

solve 2015 20 'a' _ _ = show (withMinPresents  36000000)
solve 2015 20 'b' _ _ = show (withMinPresents2 36000000)

solve 2015 21 'a' _ (cheapestVictory . into @String -> solution) = show solution
solve 2015 21 'b' _ (highestLoss     . into @String -> solution) = show solution

solve 2015 22 'a' _ (spellBattle False . into @String -> solution) = show solution
solve 2015 22 'b' _ (spellBattle True  . into @String -> solution) = show solution

solve 2015 23 'a' _ (exInstructions . into @String -> solution) = show solution
solve 2015 23 'b' _ (exInstructions2 . into @String -> solution) = show solution

solve 2015 24 'a' _ (idealEntanglement 3 . into @String -> solution) = show solution
solve 2015 24 'b' _ (idealEntanglement 4 . into @String -> solution) = show solution

solve 2015 25  _ _ (manualCodeFrom . into @String -> solution) = show solution

solve 2016 1 'a' _ (blockDistance . into @String -> solution) = show solution
solve 2016 1 'b' _ (visitedTwice  . into @String -> solution) = show solution

solve 2016 2 'a' _ (bathroomCode grid1 (2,2) . into @String -> solution) = show solution
solve 2016 2 'b' _ (bathroomCode grid2 (1,3) . into @String -> solution) = show solution

solve 2016 9 'a' _ (inflate -> Right solution) = show solution
solve 2016 9 'a' _ (inflate -> Left error') = show error'
solve 2016 9 'b' _ (nestedInflate -> solution) = show solution

solve 2016 10 'a' args input = show $ findBot (map read args) input
solve 2016 10 'b' args input = show $ findOutputs (map read args) input

solve 2018 1 'a' _ (frequency . into @String -> solution) = show solution
solve 2018 1 'b' _ (twiceFrequency . into @String -> solution) = show solution

solve 2018 2 'a' _ (checksum . into @String -> solution) = show solution
solve 2018 2 'b' _ (boxID . into @String -> solution) = show solution

solve 2018 3 'a' _ (overlappedInches . into @String -> solution) = show solution
solve 2018 3 'b' _ (intactInches . into @String -> solution) = show solution

solve 2018 4 'a' _ (laziestGuard . into @String -> solution) = show solution
solve 2018 4 'b' _ (laziestMinute . into @String -> solution) = show solution

solve 2018 5 'a' _ (react . rstrip . into @String -> solution) = show solution
solve 2018 5 'b' _ (reactBest . rstrip . into @String -> solution) = show solution

solve 2021 1 'a' _ (part1A -> solution) = show solution
solve 2021 1 'b' _ (part1B -> solution) = show solution

solve 2021 2 'a' _ (part2A -> solution) = show solution
solve 2021 2 'b' _ (part2B -> solution) = show solution

solve 2021 3 'a' _ (part3A -> solution) = show solution
solve 2021 3 'b' _ (part3B -> solution) = show solution

solve 2021 4 'a' _ (part4A -> solution) = show solution
solve 2021 4 'b' _ (part4B -> solution) = show solution

solve 2021 5 'a' _ (part5A -> solution) = show solution
solve 2021 5 'b' _ (part5B -> solution) = show solution

solve 2021 6 'a' _ (part6AMV -> solution) = show solution
solve 2021 6 'b' _ (part6BMV -> solution) = show solution

solve 2021 7 'a' _ (part7A -> solution) = show solution
solve 2021 7 'b' _ (part7B -> solution) = show solution

solve 2021 8 'a' _ (part8A -> solution) = show solution
solve 2021 8 'b' _ (part8B -> solution) = show solution

solve 2021 9 'a' _ (part9A -> solution) = show solution
solve 2021 9 'b' _ (part9B -> solution) = show solution

solve 2021 10 'a' _ (part10A -> solution) = show solution
solve 2021 10 'b' _ (part10B -> solution) = show solution

solve 2021 11 'a' _ (part11A -> solution) = show solution
solve 2021 11 'b' _ (part11B -> solution) = show solution

solve 2021 12 'a' _ (part12A -> solution) = show solution
solve 2021 12 'b' _ (part12B -> solution) = show solution

solve 2021 13 'a' _ (part13A -> solution) = show solution
solve 2021 13 'b' _ (part13B -> solution) = show solution

solve 2021 14 'a' _ (part14A -> solution) = show solution
solve 2021 14 'b' _ (part14B -> solution) = show solution

solve 2021 15 'a' _ (part15A -> solution) = show solution
solve 2021 15 'b' _ (part15B -> solution) = show solution

solve 2021 16 'a' _ (part16A -> solution) = show solution
solve 2021 16 'b' _ (part16B -> solution) = show solution

solve 2021 17 'a' _ (part17A -> solution) = show solution
solve 2021 17 'b' _ (part17B -> solution) = show solution

solve 2021 18 'a' _ (part18A -> solution) = show solution
solve 2021 18 'b' _ (part18B -> solution) = show solution

solve 2021 19 'a' _ (part19A -> solution) = show solution
solve 2021 19 'b' _ (part19B -> solution) = show solution

solve 2021 20 'a' _ (part20A -> solution) = show solution
solve 2021 20 'b' _ (part20B -> solution) = show solution

solve y d p _ _ = error $
  "I can't handle year " <> show y <> " day " <> show d <> " part " <> show p
