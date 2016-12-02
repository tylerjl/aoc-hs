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
        [ bgroup "Day 1"
            [ bgroup "blockDistance"
              [ bench "simple" $ nf blockDistance "R2, L3"
              , bench "larger" $ nf blockDistance "R5, L5, R5, R3"
              ]
            , bgroup "visitedTwice"
              [ bench "simple" $ nf visitedTwice "R8, R4, R4, R8"
              ]
            ]
        , bgroup "Day 2"
            [ bgroup "bathroomCode"
              [ bench "part 1" $ nf (bathroomCode grid1 (2,2)) bathroomDirections
              , bench "part 2" $ nf (bathroomCode grid2 (1,3)) bathroomDirections
              ]
            ]
        ]
