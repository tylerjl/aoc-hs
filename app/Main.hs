{-|
Module:      Main
Description: Advent of Code solution driver.
License:     MIT
Maintainer:  @tylerjl

Entrypoint for the CLI interface to AoC solutions in Haskell.
-}
module Main where

import Data.Text       (Text)
import Options.Generic (unwrapRecord)

import qualified Data.ByteString.Lazy as L

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
  (Options year day part path) <- unwrapRecord usage
  readFile path >>= putStrLn . solver year day part
