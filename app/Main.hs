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
  Options (Flags { timed, input }) (Arguments year day part args) <- getRecord usage

  problemText <- case input of
    Nothing -> pure Nothing
    Just p -> do
      x <- TIO.readFile p
      pure $ Just x

  let solution = solve year day part args
  if timed then do
    (Measured { measTime }, _) <- measure (nf solution problemText) 1
    putStrLn $ "Elapsed: " ++ secs measTime
  else putStrLn (solution problemText)
