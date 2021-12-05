{-# LANGUAGE OverloadedStrings #-}

module Y2021.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Witch

import Y2021.D01
import Y2021.D02
import Y2021.D03
import Y2021.D04

getInput :: IO (Text, Text, Text, (Text, Text))
getInput =
  (,,,) <$> TIO.readFile "dist/resources/2021/day1.txt"
        <*> TIO.readFile "dist/resources/2021/day2.txt"
        <*> TIO.readFile "dist/resources/2021/day3.txt"
        <*> ((,) <$> TIO.readFile "dist/resources/2021/day4_sample.txt"
                 <*> TIO.readFile "dist/resources/2021/day4.txt")

benchmarks :: Benchmark
benchmarks = env getInput $ \ ~(d1, d2, d3, (d4s, d4l)) ->
    bgroup "Y2021"
        [ bgroup "Day 1"
            [ bgroup "partA"
              [ bgroup "naive"
                [ bench "simple" $ nf partA d1sample
                , bench "larger" $ nf partA d1
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partAZip d1sample
                , bench "larger" $ nf partAZip d1
                ]
              , bgroup "recursive"
                [ bench "simple" $ nf partARecur d1sample
                , bench "larger" $ nf partARecur d1
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf partB d1sample
                , bench "larger" $ nf partB d1
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partBZip d1sample
                , bench "larger" $ nf partBZip d1
                ]
              ]
            ]
        , bgroup "Day 2"
            [ bgroup "partA"
              [ bgroup "naive"
                [ bench "simple" $ nf part2A d2sample
                , bench "larger" $ nf part2A d2
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf part2B d2sample
                , bench "larger" $ nf part2B d2
                ]
              ]
            ]
        , bgroup "Day 3"
            [ bgroup "partA"
              [ bgroup "naive"
                [ bench "simple" $ nf part3A d3sample
                , bench "larger" $ nf part3A d3
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf part3B d3sample
                , bench "larger" $ nf part3B d3
                ]
              ]
            ]
        , bgroup "Day 4"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "simple" $ nf part4A d4s
                , bench "larger" $ nf part4A d4l
                ]
              ]
            ]
        ]
