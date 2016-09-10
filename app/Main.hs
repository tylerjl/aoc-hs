module Main where

import Control.Applicative ((<*>))
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.String.Utils (rstrip)
import Safe
import System.Environment  (getArgs)
import Text.Read           (readMaybe)
import Y2015

usage :: String
usage = "Usage: <day number> [input file or string]"

run :: Int -> String -> IO ()
run 1 file = do contents <- readFile file
                print (level contents, basement contents)
run 2 file = do contents <- readFile file
                case parsePresents contents of
                     Nothing -> putStrLn $ "Could not parse" ++ file
                     Just ps -> print ( surfaceArea ps
                                      , ribbonLength ps
                                      )
run 3 file = do c <- readFile file
                let contents = filter (/= '\n') c
                print (santaRun contents, roboRun contents)
run 4 file = do contents <- B.readFile file
                print (crack contents 5, crack contents 6)
run 5 file = let check f = length . filter f . lines
             in do contents <- readFile file
                   print (check isNice contents, check isNicer contents)
run 6 file = do contents <- readFile file
                case parseInstructions contents of
                     Left e -> putStrLn $ "Could not parse: " ++ show e
                     Right insts -> do
                         a <- lightSimulation configureGridA insts
                         b <- lightSimulation configureGridB insts
                         print (a, b)
run 7 file = do contents <- readFile file
                case parseCircuits contents of
                     Left e -> putStrLn $ "Could not parse: " ++ show e
                     Right insts -> let a = wire "a" insts in
                         print
                             ( a
                             , wire "a" (insts ++ [override a])
                             )
run 8 file = do contents <- readFile file
                print (difference contents, encoded contents)
run 9 file = do contents <- readFile file
                case regularParse routeParser contents of
                     Left e -> putStrLn $ "Could not parse: " ++ show e
                     Right rs -> print (shortestRoute rs,  longestRoute rs)
run 10 file = do
    contents <- readFile file
    let stem = lookSay $ rstrip contents
    print (length $ stem 40, length $ stem 50)
run 11 file = do
    contents <- readFile file
    let pw = rotate $ rstrip contents
    print (pw, rotate pw)
run 12 file = do
    contents <- L.readFile file
    print (jsonSum contents, jsonSumFixed contents)
run 13 file = do contents <- readFile file
                 let withMe = contents
                              ++ "Yourself would gain 0 happiness units "
                              ++ "by sitting next to Yourself."
                 print (solveSeating contents, solveSeating withMe)
run 14 file = do contents <- readFile file
                 print (distanceRace contents 2503, leadingRace contents 2503)
run 15 file = do contents <- readFile file
                 print (cookieScore contents, calorieScore contents)
run 16 file = do contents <- readFile file
                 print (findAunt contents, findRealAunt contents)
run 17 file = do contents <- readFile file
                 print (150 `filledAmong` contents, 150 `minFilledAmong` contents)
run 18 file = do contents <- readFile file
                 print (animateLights contents 100, animateStuckLights contents 100)
run 19 file = do contents <- readFile file
                 print (distinctMols contents, molSteps contents)
run 20 file = print ( withMinPresents 36000000
                    , withMinPresents2 36000000
                    )
run 21 file = do contents <- readFile file
                 print (cheapestVictory contents, highestLoss contents)
run 23 file = do contents <- readFile file
                 print (exInstructions contents, exInstructions2 contents)
run _ p   = putStrLn "Not implemented yet."

main :: IO ()
main = do
    args <- getArgs
    let mDay = (readMaybe <=< headMay) args
        mInp = args `atMay` 1
    case (mDay, mInp) of
         (Nothing, _)     -> putStr $ unlines ["Error: missing day number.", usage]
         (_, Nothing)     -> putStr $ unlines ["Error: missing input.", usage]
         (Just d, Just i) -> run d i
