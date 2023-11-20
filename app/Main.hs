{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module:      Main
Description: Advent of Code solution driver.
License:     MIT
Maintainer:  @tylerjl

Entrypoint for the CLI interface to AoC solutions in Haskell.
-}
module Main where

import Data.Text       (Text)
import Options.Generic (getRecord)
import System.TimeIt

import qualified Data.Text.IO as TIO

import AoC
import Options
import Text.Printf (printf)

-- |CLI boilerplate
usage :: Text
usage = "Advent of Code solutions in Haskell"

humanSec :: Double -> (Double, String)
humanSec seconds
  | seconds * (10 ** ms) >= 1 = (seconds * 10 ** ms, "ms")
  | seconds * (10 ** mu) >= 1 = (seconds * 10 ** mu, "µs")
  | seconds * (10 ** ns) >= 1 = (seconds * 10 ** ns, "ns")
  | seconds * (10 ** ps) >= 1 = (seconds * 10 ** ps, "ps")
  | otherwise                 = (seconds,            "s")
  where
    (ms, mu, ns, ps) = (3, 6, 9, 12)

humanize :: Double -> String
humanize (humanSec -> (sec, unit)) = printf "%.2f%s" sec unit

{-|For use of the solutions here outside of a repl if you'd rather build and
 invoke the solution functions.
-}
main :: IO ()
main = do
  Options (Flags { measure }) (Arguments year day part path) <- getRecord usage
  input <- TIO.readFile path 
  let solution = solve year day part input
  if measure then do
    (!elapsed, !output) <- timeItT (pure solution)
    putStrLn $ "Elapsed: " ++ humanize elapsed
    putStrLn output 
  else putStrLn solution
