{-# LANGUAGE OverloadedStrings #-}

module Y2015.Bench where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Criterion.Main (env)
import Control.Monad
import Data.ByteString (pack)
import Data.ByteString.Char8 (unpack)
import Data.Word (Word8)
import System.Random.MWC

import Y2015

entropy :: IO String
entropy = do
    rand <- withSystemRandom .  asGenIO $ \ gen ->
        replicateM 1000 $ uniformR (c '(', c ')') gen
    return $ unpack $ pack rand
    where c :: Char -> Word8
          c = fromIntegral . fromEnum

benchmarks :: Benchmark
benchmarks =
    env entropy $ \levels ->
        bgroup "Y2015"
            [ bgroup "D01"
              [ bgroup "partA"
                [ bench "small" $ nf level "()((()(())))(((((((())))))))(()))))))"
                , bench "large"  $ nf level (Prelude.take 100 levels)
                , bench "huge"   $ nf level levels
                ]
              ]
            , bgroup "D22"
              [ bgroup "partA"
                [ bench "small" $ whnf (testSpellBattle False)
                  (unlines ["Hit Points: 13", "Damage: 8"])
                , bench "small'" $ whnf (testSpellBattle False)
                  (unlines ["Hit Points: 14", "Damage: 8"])
                ]
              ]
            , bgroup "D24"
                [ bgroup "partA"
                    [ bench "small" $ whnf (idealEntanglement 3)
                        (unlines $ map show [1, 3, 2, 2, 4])
                    , bench "large" $ whnf (idealEntanglement 3)
                        (unlines $ map show $ [1..5] ++ [7..11])
                    ]
                , bgroup "partB"
                    [ bench "small" $ whnf (idealEntanglementOptimized 3)
                        (unlines $ map show [1, 3, 2, 2, 4])
                    , bench "large" $ whnf (idealEntanglementOptimized 3)
                        (unlines $ map show $ [1..5] ++ [7..11])
                    ]
                ]
            ]
