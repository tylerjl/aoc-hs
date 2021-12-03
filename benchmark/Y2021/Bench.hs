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

d2sample :: Text
d2sample =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ] & T.unlines

getInput :: IO (Text, Text, Text)
getInput =
  (,,) <$> TIO.readFile "dist/resources/2021/day1.txt"
       <*> TIO.readFile "dist/resources/2021/day2.txt"
       <*> TIO.readFile "dist/resources/2021/day3.txt"

benchmarks :: Benchmark
benchmarks = env getInput $ \ ~(d1, d2, d3) ->
    bgroup "Y2021"
        [ bgroup "Day 1"
            [ bgroup "partA"
              [ bgroup "naive"
                [ bench "simple" $ nf partA sample
                , bench "larger" $ nf partA d1
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partAZip sample
                , bench "larger" $ nf partAZip d1
                ]
              , bgroup "recursive"
                [ bench "simple" $ nf partARecur sample
                , bench "larger" $ nf partARecur d1
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf partB sample
                , bench "larger" $ nf partB d1
                ]
              , bgroup "zipped"
                [ bench "simple" $ nf partBZip sample
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
                [ bench "simple" $ nf part3A Y2021.sample3
                , bench "larger" $ nf part3A d3
                ]
              ]
            , bgroup "partB"
              [ bgroup "naive"
                [ bench "simple" $ nf part3B Y2021.sample3
                , bench "larger" $ nf part3B d3
                ]
              ]
            ]
        ]
