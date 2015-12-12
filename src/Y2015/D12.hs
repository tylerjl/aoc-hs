#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Y2015.D12 (jsonSum, jsonSumFixed) where

import           Data.Aeson (Array(..), Object(..), Value(..), decode)
import           Data.ByteString.Lazy (ByteString, readFile)
import qualified Data.ByteString.Lazy as L
import           Data.HashMap.Strict (HashMap(..))
import           Data.HashMap.Strict  as M
import           Data.Scientific      (floatingOrInteger)
import           Data.Vector          as V hiding (any)

jsonSum :: ByteString -> Int
jsonSum = jSum . decode
    where jSum Nothing  = 0
          jSum (Just v) = sumValue v

jsonSumFixed :: ByteString -> Int
jsonSumFixed = jSum . decode
    where jSum Nothing  = 0
          jSum (Just v) = sumValue $ filterV v

filterV :: Value -> Value
filterV o@(Object x) | isRed o    = Null
                     | otherwise  = Object (M.map filterV x)
filterV a@(Array v)               = Array (V.map filterV v)
filterV s@(String x) | x == "red" = Null
                     | otherwise  = s
filterV v                         = v

isRed :: Value -> Bool
isRed (String x) = x == "red"
isRed (Object o) = any isRed $ Prelude.filter isString $ M.elems o
isRed _          = False

isString :: Value -> Bool
isString (String _) = True
isString _          = False

valAcc :: Int -> Value -> Int
valAcc n v = n + sumValue v

sumValue :: Value -> Int
sumValue (Object o) = M.foldl' valAcc 0 o
sumValue (Number n) = case floatingOrInteger n of
                           Left _  -> 0
                           Right i -> i
sumValue (Array n)  = V.foldl' valAcc 0 n
sumValue _          = 0

main :: IO ()
main = do
    input <- L.readFile "src/Y2015/D12_input"
    putStr "Part A - total sum is: "
    print $ jsonSum input
    putStr "Part B - total sum without red objects is: "
    print $ jsonSumFixed input
