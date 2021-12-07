{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Y2021.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Data.Function ((&))
import Data.Text (Text)
import Witch

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Y2021

getInput :: IO (Text, Text, Text, (Text, Text), (Text, Text), (Text, Text))
getInput =
  (,,,,,) <$> TIO.readFile "dist/resources/2021/day1.txt"
          <*> TIO.readFile "dist/resources/2021/day2.txt"
          <*> TIO.readFile "dist/resources/2021/day3.txt"
          <*> ((,) <$> TIO.readFile "dist/resources/2021/day4_sample.txt"
                   <*> TIO.readFile "dist/resources/2021/day4.txt")
          <*> ((,) <$> TIO.readFile "dist/resources/2021/day5_sample.txt"
                   <*> TIO.readFile "dist/resources/2021/day5.txt")
          <*> ((,) <$> TIO.readFile "dist/resources/2021/day6_sample.txt"
                   <*> TIO.readFile "dist/resources/2021/day6.txt")

benchmarks :: Benchmark
benchmarks = env getInput $ \ ~(d1, d2, d3, (d4s, d4l), (d5s, d5l), (d6s, d6l)) ->
    bgroup "Y2021"
        [ bgroup "D01"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ nf partA d1sample
                , bench "large" $ nf partA d1
                ]
              , bgroup "zipped"
                [ bench "small" $ nf partAZip d1sample
                , bench "large" $ nf partAZip d1
                ]
              , bgroup "recursive"
                [ bench "small" $ nf partARecur d1sample
                , bench "large" $ nf partARecur d1
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ nf partB d1sample
                , bench "large" $ nf partB d1
                ]
              , bgroup "zipped"
                [ bench "small" $ nf partBZip d1sample
                , bench "large" $ nf partBZip d1
                ]
              ]
            ]
        , bgroup "D02"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ nf part2A d2sample
                , bench "large" $ nf part2A d2
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ nf part2B d2sample
                , bench "large" $ nf part2B d2
                ]
              ]
            ]
        , bgroup "D03"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ nf part3A d3sample
                , bench "large" $ nf part3A d3
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ nf part3B d3sample
                , bench "large" $ nf part3B d3
                ]
              ]
            ]
        , bgroup "D04"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ whnf part4A d4s
                , bench "large" $ whnf part4A d4l
                ]
              , bgroup "set"
                [ bench "small" $ whnf part4ASet d4s
                , bench "large" $ whnf part4ASet d4l
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ whnf part4B d4s
                , bench "large" $ whnf part4B d4l
                ]
              , bgroup "set"
                [ bench "small" $ whnf part4BSet d4s
                , bench "large" $ whnf part4BSet d4l
                ]
              ]
            ]
        , bgroup "D05"
            [ bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ nf part5A d5s
                , bench "large" $ nf part5A d5l
                ]
              , bgroup "hashmap"
                [ bench "small" $ nf part5AHM d5s
                , bench "large" $ nf part5AHM d5l
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ nf part5B d5s
                , bench "large" $ nf part5B d5l
                ]
              , bgroup "hashmap"
                [ bench "small" $ nf part5BHM d5s
                , bench "large" $ nf part5BHM d5l
                ]
              ]
            ]
        , bgroup "D06"
            [ bgroup "parser"
              [ bgroup "parsec"
                [ bench "small" $ nf parseFish d6s
                , bench "large" $ nf parseFish d6l
                ]
              , bgroup "attoparsec"
                [ bench "small" $ nf parseFish'' d6s
                , bench "large" $ nf parseFish'' d6l
                ]
              , bgroup "read"
                [ bench "small" $ nf parseFish' d6s
                , bench "large" $ nf parseFish' d6l
                ]
              ]
            , bgroup "partA"
              [ bgroup "initial"
                [ bench "small" $ nf part6A d6s
                , bench "large" $ nf part6A d6l
                ]
              , bgroup "vectors"
                [ bench "small" $ nf part6AMV d6s
                , bgroup "large"
                  [ bench "unparsed" $ nf part6AMV d6l
                  , env (pure $ parseFish d6l)
                    $ bench "pre-parsed" . nf (solve6MV 80)
                  ]
                ]
              , bgroup "seq"
                [ bench "small" $ nf part6ASeq d6s
                , bench "large" $ nf part6ASeq d6l
                ]
              ]
            , bgroup "partB"
              [ bgroup "initial"
                [ bench "small" $ nf part6B d6s
                , bench "large" $ nf part6B d6l
                ]
              , bgroup "vectors"
                [ bench "small" $ nf part6BMV d6s
                , bench "large" $ nf part6BMV d6l
                ]
              , bgroup "seq"
                [ bench "small" $ nf part6BSeq d6s
                , bench "large" $ nf part6BSeq d6l
                ]
              ]
            ]
        ]
