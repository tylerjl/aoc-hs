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
            [ bgroup "hashmap"
              [ bench "large" $ whnf part5A sample ]
            ]
          , bgroup "B"
            [ bgroup "hashmap"
              [ bench "large" $ whnf part5B sample ]
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
    , env (getProblem "8") \sample -> do
        bgroup "D08"
          [ bgroup "A"
            [ bgroup "initial"
              [ env (pure $ parse8 sample)
                  $ bench "pre-parsed" . whnf solve8A
              , bench "large" $ whnf part8A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part8B sample ]
            ]
          ]
    , env (getProblem "9") \sample -> do
        bgroup "D09"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part9A sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part9B sample ]
            ]
          ]
    , env (getProblem "10") \sample -> do
        bgroup "D10"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part10A sample ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part10B sample ]
            ]
          ]
    , env (getProblem "11") \sample -> do
        bgroup "D11"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part11A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part11B sample
              ]
            ]
          ]
    , env (getProblem "12") \sample -> do
        bgroup "D12"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part12A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part12B sample
              ]
            ]
          ]
    , env (getProblem "13") \sample -> do
        bgroup "D13"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part13A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ nf part13B sample
              ]
            ]
          ]
    , env (getProblem "14") \sample -> do
        bgroup "D14"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part14A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part14B sample
              ]
            ]
          ]
    , env (getProblem "15") \sample -> do
        bgroup "D15"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part15A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part15B sample
              ]
            ]
          ]
    , env (getProblem "16") \sample -> do
        bgroup "D16"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part16A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part16B sample
              ]
            ]
          ]
    , env (getProblem "17") \sample -> do
        bgroup "D17"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part17A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part17B sample
              ]
            ]
          ]
    , env (getProblem "18") \sample -> do
        bgroup "D18"
          [ bgroup "A"
            [ bgroup "initial"
              [ bench "large" $ whnf part18A sample
              ]
            ]
          , bgroup "B"
            [ bgroup "initial"
              [ bench "large" $ whnf part18B sample
              ]
            ]
          ]
    ]
