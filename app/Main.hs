{-# LANGUAGE NamedFieldPuns #-}
{-|
Module:      Main
Description: Advent of Code solution driver.
License:     MIT
Maintainer:  @tylerjl

Entrypoint for the CLI interface to AoC solutions in Haskell.
-}
module Main where

import Criterion.Measurement
import Criterion.Measurement.Types hiding (measure)
import Data.Text       (Text)
import Options.Generic (getRecord)

import qualified Data.Text.IO as TIO

import AoC
import Options

-- |CLI boilerplate
usage :: Text
usage = "Advent of Code solutions in Haskell"

{-|For use of the solutions here outside of a repl if you'd rather build and
 invoke the solution functions.
-}
main :: IO ()
main = do
  Options (Flags { timed }) (Arguments year day part path) <- getRecord usage
  input <- TIO.readFile path 
  let solution = solve year day part
  if timed then do
    (Measured { measTime }, _) <- measure (nf solution input) 1
    putStrLn $ "Elapsed: " ++ secs measTime
  else putStrLn (solution input)
