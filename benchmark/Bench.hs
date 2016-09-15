module Main (main) where

import           Criterion.Main (bench, bgroup, defaultMain)
import qualified Y2015.Bench

main :: IO ()
main = defaultMain
    [ Y2015.Bench.benchmarks
    ]
