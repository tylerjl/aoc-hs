{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Y2023.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Data.Function ((&))
import Data.Text (Text)
import Witch

import Y2023

getProblemString :: FilePath -> IO String
getProblemString n = readFile $ "dist/resources/2023/day" <> n <> ".txt"

benchmarks :: Benchmark
benchmarks =
  bgroup "Y2023"
    [ env (getProblemString "1") $ \sample -> do
        bgroup "D01"
          [ bgroup "A" [ bench "large" $ nf partA sample ]
          , bgroup "B" [ bench "large" $ nf partB sample ]
          ]
    ]
