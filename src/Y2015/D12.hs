{-|
Module:      Y2015.D12
Description: Advent of Code Day 12 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 12 set of problems for <adventofcode.com>.
-}

{-# LANGUAGE OverloadedStrings #-}

module Y2015.D12 (jsonSum, jsonSumFixed) where

import           Data.Aeson           (Array(..), Object(..), Value(..), decode)
import           Data.ByteString.Lazy (ByteString, readFile)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as M
import           Data.Scientific      (floatingOrInteger)
import qualified Data.Vector          as V

-- |Sum all numbers in a JSON-like structure
jsonSum :: ByteString -- ^ JSON input string
        -> Int        -- ^ Summation of all nested numeric values
jsonSum = jSum . decode
    where jSum Nothing  = 0
          jSum (Just v) = sumValue v

-- |Sums all numbers in a JSON-like structure with a specific filter
jsonSumFixed :: ByteString -- ^ JSON input string
             -> Int        -- ^ Summation of all nested numeric values
jsonSumFixed = jSum . decode
    where jSum Nothing  = 0
          jSum (Just v) = sumValue $ filterV v

filterV :: Value -> Value
filterV o@(Object x) | r o    = Null
                     | otherwise  = Object (M.map filterV x)
                     where r (String x) = x == "red"
                           r (Object o) = any r $ filter string $ M.elems o
                           r _          = False
                           string (String _) = True
                           string _          = False
filterV a@(Array v)               = Array (V.map filterV v)
filterV s@(String x) | x == "red" = Null
                     | otherwise  = s
filterV v                         = v

sumValue :: Value -> Int
sumValue (Object o) = M.foldl' valAcc 0 o
sumValue (Number n) = case floatingOrInteger n of
                           Left _  -> 0
                           Right i -> i
sumValue (Array n)  = V.foldl' valAcc 0 n
sumValue _          = 0

valAcc :: Int -> Value -> Int
valAcc = flip ((+) . sumValue)
