{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Y2023.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Witch

import Y2023

inputPath :: FilePath -> FilePath
inputPath n = "dist/resources/2023/day" <> n <> ".txt"

getProblemString :: FilePath -> IO String
getProblemString = readFile . inputPath

getProblemText :: FilePath -> IO Text
getProblemText = TIO.readFile . inputPath

benchmarks :: Benchmark
benchmarks =
  bgroup "Y2023"
    [ env (getProblemString "1") $ \sample -> do
        bgroup "D01"
          [ bgroup "A" [ bench "large" $ nf part1A sample ]
          , bgroup "B" [ bench "large" $ nf part1B sample ]
          ]
    , env (getProblemText "2") $ \sample -> do
        bgroup "D02"
          [ bgroup "A" [ bench "large" $ nf part2A sample ]
          , bgroup "B" [ bench "large" $ nf part2B sample ]
          ]
    , env (getProblemText "3") $ \sample -> do
        bgroup "D03"
          [ bgroup "A" [ bench "large" $ nf part3A sample ]
          , bgroup "B" [ bench "large" $ nf part3B sample ]
          ]
    , env (getProblemText "4") $ \sample -> do
        bgroup "D04"
          [ bgroup "A" [ bench "large" $ nf part4A sample ]
          , bgroup "B" [ bench "large" $ nf part4B sample ]
          ]
    ]
