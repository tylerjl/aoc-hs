module Main where

import Control.Applicative ((<*>))
import Control.Monad
import qualified Data.ByteString as B
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
run 13 file = do contents <- readFile file
                 let withMe = contents
                              ++ "Yourself would gain 0 happiness units "
                              ++ "by sitting next to Yourself."
                 print $ (solveSeating contents, solveSeating withMe)
run 14 file = do contents <- readFile file
                 print (distanceRace contents 2503, leadingRace contents 2503)
run 15 file = do contents <- readFile file
                 print (cookieScore contents, calorieScore contents)
run 16 file = do contents <- readFile file
                 print (findAunt contents, findRealAunt contents)
run _ p   = putStrLn "Not implemented yet."

main :: IO ()
main = do
    args <- getArgs
    let mDay = (readMaybe <=< headMay) args
        mInp = args `atMay` 1
    case (mDay, mInp) of
         (Nothing, _)     -> putStr $ unlines $ ["Error: missing day number.", usage]
         (_, Nothing)     -> putStr $ unlines $ ["Error: missing input.", usage]
         (Just d, Just i) -> run d i
