module Main (main) where

import           Criterion.Main (bench, bgroup, defaultMain)
import qualified Y2015.Bench
import qualified Y2016.Bench
import qualified Y2021.Bench

main :: IO ()
main = defaultMain
    [ Y2015.Bench.benchmarks
    , Y2016.Bench.benchmarks
    , Y2021.Bench.benchmarks
    ]
