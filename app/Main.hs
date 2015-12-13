module Main where

import Control.Applicative ((<*>))
import Control.Exception   (try)
import Control.Monad
import Safe
import System.Environment  (getArgs)
import Text.Read           (readMaybe)
import Y2015.D01

usage :: String
usage = "Usage: <day number> [input file or string]"

run :: Int -> String -> IO ()
run 0 file = do
    input <- try $ readFile file :: IO (Either IOError String)
    case input of
         Left exception -> print $ exception
         Right contents -> print $ (level contents, basement contents)
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
