{-# LANGUAGE OverloadedStrings #-}

module Y2016.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)

import Y2016

bathroomDirections = unlines
    [ "ULL"
    , "RRDDD"
    , "LURDL"
    , "UUUUD"
    ]

benchmarks :: Benchmark
benchmarks =
    bgroup "Y2016"
        [ bgroup "D01"
            [ bgroup "partA"
              [ bench "small" $ nf blockDistance "R2, L3"
              , bench "large" $ nf blockDistance "R5, L5, R5, R3"
              ]
            , bgroup "partB"
              [ bench "small" $ nf visitedTwice "R8, R4, R4, R8"
              ]
            ]
        , bgroup "D02"
          [ bgroup "partA"
            [ bench "small" $ nf (bathroomCode grid1 (2,2)) bathroomDirections
            ]
          , bgroup "partB"
            [ bench "small" $ nf (bathroomCode grid2 (1,3)) bathroomDirections
            ]
          ]
        ]
