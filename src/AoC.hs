module AoC
  ( solve
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.String.Utils (rstrip)
import Data.Text (Text)
import GHC.Utils.Misc (split)
import Witch
import Y2015
import Y2016
import Y2018
import Y2021
import qualified Y2023

import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T

{-|Is there a nicer way to "dynamically" invoke these module functions based on
 year/day input? Maybe. But a big fat pattern match works, too. The small bit of
 glue between the outside world Main call and the solver functions.
-}
solve :: Int -> Int -> Char -> [String] -> Maybe Text -> String
solve 2015 1 'a' _ (Just (level -> solution)) = show solution
solve 2015 1 'b' _ (Just (basement -> solution)) = show solution

solve 2015 2 'a' _ (Just (fmap surfaceArea  . parsePresents -> Just solution)) = show solution
solve 2015 2 'b' _ (Just (fmap ribbonLength . parsePresents -> Just solution)) = show solution
solve 2015 2  _  _ (Just (parsePresents . into @Text -> Nothing)) = error "could not parse file"

solve 2015 3 'a' _ (Just (santaRun . T.filter (/= '\n') -> solution)) = show solution
solve 2015 3 'b' _ (Just (roboRun  . T.filter (/= '\n') -> solution)) = show solution

solve 2015 4 'a' _ (Just (flip crack 5 . into . into @(UTF_8 ByteString) -> solution)) = show solution
solve 2015 4 'b' _ (Just (flip crack 6 . into . into @(UTF_8 ByteString) -> solution)) = show solution

solve 2015 5 'a' _ (Just (length . filter isNice  . lines . into @String -> solution)) = show solution
solve 2015 5 'b' _ (Just (length . filter isNicer . lines . into @String -> solution)) = show solution

solve 2015 6 'a' _ (Just (lightSimulation configureGridA . parseInstructions . into @String -> solution)) = show solution
solve 2015 6 'b' _ (Just (lightSimulation configureGridB . parseInstructions . into @String -> solution)) = show solution

solve 2015 7 'a' _ (Just (wire "a" . parseCircuits . into @String -> solution)) = show solution
solve 2015 7 'b' _ (Just (parseCircuits . into @String -> cs)) = show $ aSolution (cs ++ [override (aSolution cs)])
  where aSolution = wire "a"

solve 2015 8 'a' _ (Just (difference . into @String -> solution)) = show solution
solve 2015 8 'b' _ (Just (encoded    . into @String -> solution)) = show solution

solve 2015 9 'a' _ (Just (regularParse routeParser . into @String -> Right rs)) = show $ shortestRoute rs
solve 2015 9 'b' _ (Just (regularParse routeParser . into @String -> Right rs)) = show $ longestRoute rs
solve 2015 9 _   _ (Just (regularParse routeParser . into @String -> Left err)) = error (show err)

solve 2015 10 'a' _ (Just (length . flip lookSay 40 . rstrip . into @String -> solution)) = show solution
solve 2015 10 'b' _ (Just (length . flip lookSay 50 . rstrip . into @String -> solution)) = show solution

solve 2015 11 'a' _ (Just (rotate . rstrip . into @String -> solution)) = show solution
solve 2015 11 'b' _ (Just (rotate . rotate . rstrip . into @String -> solution)) = show solution

solve 2015 12 'a' _ (Just (jsonSum . into . into @(UTF_8 L.ByteString) -> solution)) = show solution
solve 2015 12 'b' _ (Just (jsonSumFixed . into . into @(UTF_8 L.ByteString) -> solution)) = show solution

solve 2015 13 'a' _ (Just (solveSeating . into @String -> solution)) = show solution
solve 2015 13 'b' _ (Just (into @String -> input)) = show $ solveSeating withMe
  where withMe = input ++
         "Yourself would gain 0 happiness units " ++
         "by sitting next to Yourself."

solve 2015 14 'a' _ (Just (flip distanceRace 2503 . into @String -> solution)) = show solution
solve 2015 14 'b' _ (Just (flip leadingRace 2503 . into @String -> solution)) = show solution

solve 2015 15 'a' _ (Just (cookieScore  . into @String -> solution)) = show solution
solve 2015 15 'b' _ (Just (calorieScore . into @String -> solution)) = show solution

solve 2015 16 'a' _ (Just (findAunt     . into @String -> solution)) = show solution
solve 2015 16 'b' _ (Just (findRealAunt . into @String -> solution)) = show solution

solve 2015 17 'a' _ (Just (filledAmong 150 . into @String -> solution)) = show solution
solve 2015 17 'b' _ (Just (minFilledAmong 150 . into @String -> solution)) = show solution

solve 2015 18 'a' _ (Just (flip animateLights 100 . into @String -> solution)) = show solution
solve 2015 18 'b' _ (Just (flip animateStuckLights 100 . into @String -> solution)) = show solution

solve 2015 19 'a' _ (Just (distinctMols . into @String -> solution)) = show solution
solve 2015 19 'b' _ (Just (molSteps . into @String -> solution)) = show solution

solve 2015 20 'a' _ _ = show (withMinPresents  36000000)
solve 2015 20 'b' _ _ = show (withMinPresents2 36000000)

solve 2015 21 'a' _ (Just (cheapestVictory . into @String -> solution)) = show solution
solve 2015 21 'b' _ (Just (highestLoss     . into @String -> solution)) = show solution

solve 2015 22 'a' _ (Just (spellBattle False . into @String -> solution)) = show solution
solve 2015 22 'b' _ (Just (spellBattle True  . into @String -> solution)) = show solution

solve 2015 23 'a' _ (Just (exInstructions . into @String -> solution)) = show solution
solve 2015 23 'b' _ (Just (exInstructions2 . into @String -> solution)) = show solution

solve 2015 24 'a' _ (Just (idealEntanglement 3 . into @String -> solution)) = show solution
solve 2015 24 'b' _ (Just (idealEntanglement 4 . into @String -> solution)) = show solution

solve 2015 25  _ _ (Just (manualCodeFrom . into @String -> solution)) = show solution

solve 2016 1 'a' _ (Just (blockDistance . into @String -> solution)) = show solution
solve 2016 1 'b' _ (Just (visitedTwice  . into @String -> solution)) = show solution

solve 2016 2 'a' _ (Just (bathroomCode grid1 (2,2) . into @String -> solution)) = show solution
solve 2016 2 'b' _ (Just (bathroomCode grid2 (1,3) . into @String -> solution)) = show solution

solve 2016 9 'a' _ (Just (inflate -> Right solution)) = show solution
solve 2016 9 'a' _ (Just (inflate -> Left error')) = show error'
solve 2016 9 'b' _ (Just (nestedInflate -> solution)) = show solution

solve 2016 10 'a' args (Just input) = show $ findBot (map read args) input
solve 2016 10 'b' args (Just input) = show $ findOutputs (map read args) input

solve 2016 12 'a' [[register]] (Just input) = show $ assembunnyRegister register input
solve 2016 12 'b' [[register]] (Just input) = show $ assembunnyRegisterInit cOne register input
  where cOne r = if r == 'c' then 1 else 0

solve 2016 13 'a' [seed, split ',' -> [x, y]] _
  = show $ officePath (read seed) (read x, read y)

solve 2016 13 'b' [seed, steps] _
  = show $ floodOffice (read seed) (read steps)

solve 2018 1 'a' _ (Just (frequency . into @String -> solution)) = show solution
solve 2018 1 'b' _ (Just (twiceFrequency . into @String -> solution)) = show solution

solve 2018 2 'a' _ (Just (checksum . into @String -> solution)) = show solution
solve 2018 2 'b' _ (Just (boxID . into @String -> solution)) = show solution

solve 2018 3 'a' _ (Just (overlappedInches . into @String -> solution)) = show solution
solve 2018 3 'b' _ (Just (intactInches . into @String -> solution)) = show solution

solve 2018 4 'a' _ (Just (laziestGuard . into @String -> solution)) = show solution
solve 2018 4 'b' _ (Just (laziestMinute . into @String -> solution)) = show solution

solve 2018 5 'a' _ (Just (react . rstrip . into @String -> solution)) = show solution
solve 2018 5 'b' _ (Just (reactBest . rstrip . into @String -> solution)) = show solution

solve 2021 1 'a' _ (Just (part1A -> solution)) = show solution
solve 2021 1 'b' _ (Just (part1B -> solution)) = show solution

solve 2021 2 'a' _ (Just (part2A -> solution)) = show solution
solve 2021 2 'b' _ (Just (part2B -> solution)) = show solution

solve 2021 3 'a' _ (Just (part3A -> solution)) = show solution
solve 2021 3 'b' _ (Just (part3B -> solution)) = show solution

solve 2021 4 'a' _ (Just (part4A -> solution)) = show solution
solve 2021 4 'b' _ (Just (part4B -> solution)) = show solution

solve 2021 5 'a' _ (Just (part5A -> solution)) = show solution
solve 2021 5 'b' _ (Just (part5B -> solution)) = show solution

solve 2021 6 'a' _ (Just (part6AMV -> solution)) = show solution
solve 2021 6 'b' _ (Just (part6BMV -> solution)) = show solution

solve 2021 7 'a' _ (Just (part7A -> solution)) = show solution
solve 2021 7 'b' _ (Just (part7B -> solution)) = show solution

solve 2021 8 'a' _ (Just (part8A -> solution)) = show solution
solve 2021 8 'b' _ (Just (part8B -> solution)) = show solution

solve 2021 9 'a' _ (Just (part9A -> solution)) = show solution
solve 2021 9 'b' _ (Just (part9B -> solution)) = show solution

solve 2021 10 'a' _ (Just (part10A -> solution)) = show solution
solve 2021 10 'b' _ (Just (part10B -> solution)) = show solution

solve 2021 11 'a' _ (Just (part11A -> solution)) = show solution
solve 2021 11 'b' _ (Just (part11B -> solution)) = show solution

solve 2021 12 'a' _ (Just (part12A -> solution)) = show solution
solve 2021 12 'b' _ (Just (part12B -> solution)) = show solution

solve 2021 13 'a' _ (Just (part13A -> solution)) = show solution
solve 2021 13 'b' _ (Just (part13B -> solution)) = show solution

solve 2021 14 'a' _ (Just (part14A -> solution)) = show solution
solve 2021 14 'b' _ (Just (part14B -> solution)) = show solution

solve 2021 15 'a' ["vector"] (Just (part15A' -> solution)) = show solution
solve 2021 15 'a' _ (Just (part15A -> solution)) = show solution
solve 2021 15 'b' ["vector"] (Just (part15B' -> solution)) = show solution
solve 2021 15 'b' _ (Just (part15B -> solution)) = show solution

solve 2021 16 'a' _ (Just (part16A -> solution)) = show solution
solve 2021 16 'b' _ (Just (part16B -> solution)) = show solution

solve 2021 17 'a' _ (Just (part17A -> solution)) = show solution
solve 2021 17 'b' _ (Just (part17B -> solution)) = show solution

solve 2021 18 'a' _ (Just (part18A -> solution)) = show solution
solve 2021 18 'b' _ (Just (part18B -> solution)) = show solution

solve 2021 19 'a' _ (Just (part19A -> solution)) = show solution
solve 2021 19 'b' _ (Just (part19B -> solution)) = show solution

solve 2021 20 'a' _ (Just (part20A -> solution)) = show solution
solve 2021 20 'b' _ (Just (part20B -> solution)) = show solution

solve 2023 1 'a' _ (Just (Y2023.partA . into @String -> solution)) = show solution
solve 2023 1 'b' _ (Just (Y2023.partB . into @String -> solution)) = show solution

solve y d p _ _ = error $
  "I can't handle year " <> show y <> " day " <> show d <> " part " <> show p
