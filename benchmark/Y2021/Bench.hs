{-# LANGUAGE OverloadedStrings #-}

module Y2021.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Witch

import Y2021

sample :: Text
sample = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  & map (into @Text . show) & T.unlines

getInput :: IO Text
getInput = TIO.readFile "dist/resources/2021/day1.txt"

benchmarks :: Benchmark
benchmarks = env getInput $ \ ~largeInput ->
    bgroup "Y2021"
        [ bgroup "Day 1"
            [ bgroup "partA"
              [ bgroup "naive"
                [ bench "simple" $ nf partA sample
                , bench "larger" $ nf partA largeInput
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partAZip sample
                , bench "larger" $ nf partAZip largeInput
                ]
              , bgroup "recursive"
                [ bench "simple" $ nf partARecur sample
                , bench "larger" $ nf partARecur largeInput
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf partB sample
                , bench "larger" $ nf partB largeInput
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partBZip sample
                , bench "larger" $ nf partBZip largeInput
                ]
              ]
            ]
        ]
