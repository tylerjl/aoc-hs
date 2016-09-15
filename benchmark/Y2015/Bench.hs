{-# LANGUAGE OverloadedStrings #-}

module Y2015.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Control.Monad
import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.Word (Word8)
import System.Random.MWC

import Y2015

entropy :: IO String
entropy = do
    rand <- withSystemRandom .  asGenIO $ \ gen ->
        replicateM 1000 $ uniformR (c '(', c ')') gen
    return $ C.unpack $ B.pack rand
    where c :: Char -> Word8
          c = fromIntegral . fromEnum

benchmarks :: Benchmark
benchmarks =
    env entropy $ \levels ->
        bgroup "Y2015"
            [ bgroup "Day 1"
                [ bench "simple" $ nf level "()((()(())))(((((((())))))))(()))))))"
                , bench "large"  $ nf level (Prelude.take 100 levels)
                , bench "huge"   $ nf level levels
                ]
            , bgroup "Day 22"
                [ bench "simple" $ whnf (testSpellBattle False)
                    (Prelude.unlines ["Hit Points: 13", "Damage: 8"])
                , bench "alternate" $ whnf (testSpellBattle False)
                    (Prelude.unlines ["Hit Points: 14", "Damage: 8"])
                ]
            ]
