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

getProblem :: FilePath -> IO Text
getProblem n = TIO.readFile $ "dist/resources/2021/day" <> n <> ".txt"

benchmarks :: Benchmark
benchmarks =
  bgroup "Y2021"
    [ env (getProblem "1") $ \sample -> do
        bgroup "D01"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ nf part1A sample ]
            , bgroup "zipped"
              [ bench "large" $ nf part1AZip sample ]
            , bgroup "recursive"
              [ bench "large" $ nf part1ARecur sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part1B sample ]
            , bgroup "zipped"
              [ bench "large" $ nf part1BZip sample ]
            ]
          ]
    , env (getProblem "2") $ \sample -> do
        bgroup "D02"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ nf part2A sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part2B sample ]
            ]
          ]
    , env (getProblem "3") $ \sample -> do
        bgroup "D03"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ nf part3A sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part3B sample ]
            ]
          ]
    , env (getProblem "4") $ \sample -> do
        bgroup "D04"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part4A sample ]
            , bgroup "set"
              [ bench "large" $ whnf part4ASet sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part4B sample ]
            , bgroup "set"
              [ bench "large" $ whnf part4BSet sample ]
            ]
          ]
    , env (getProblem "5") $ \sample -> do
        bgroup "D05"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ nf part5A sample ]
            , bgroup "hashmap"
              [ bench "large" $ nf part5AHM sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part5B sample ]
            , bgroup "hashmap"
              [ bench "large" $ nf part5BHM sample ]
            ]
          ]
    , env (getProblem "6") \sample -> do
        bgroup "D06"
          [ bgroup "parser"
            [ bgroup "parsec"
              [ bench "large" $ nf parseFish sample ]
            , bgroup "attoparsec"
              [ bench "large" $ nf parseFish'' sample ]
            , bgroup "read"
              [ bench "large" $ nf parseFish' sample ]
            ]
          , bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ nf part6A sample ]
            , bgroup "vectors"
              [ bgroup "large"
                [ bench "unparsed" $ nf part6AMV sample
                , env (pure $ parseFish sample)
                  $ bench "pre-parsed" . nf (solve6MV 80)
                ]
              ]
            , bgroup "seq"
              [ bench "large" $ nf part6ASeq sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part6B sample ]
            , bgroup "vectors"
              [ bench "large" $ nf part6BMV sample ]
            , bgroup "seq"
              [ bench "large" $ nf part6BSeq sample ]
            ]
          ]
    , env (getProblem "7") \sample -> do
        bgroup "D07"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part7A sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part7B sample ]
            ]
          ]
    ]
