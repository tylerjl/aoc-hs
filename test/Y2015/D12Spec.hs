module Y2015.D12Spec (spec) where

{-# LANGUAGE OverloadedStrings #-}

import Y2015
import Test.Hspec
import Data.ByteString.Lazy.Char8 (pack)

spec :: Spec
spec = parallel $ do
    describe "Day 12" $ do
        describe "jsonSum" $ do
            it "sums three-member lists" $
                jsonSum (pack "[1,2,3]") `shouldBe` 6
            it "sums two-member objects" $
                jsonSum (pack "{\"a\":2,\"b\":4}") `shouldBe` 6
            it "sums nested lists" $
                jsonSum (pack "[[[3]]]") `shouldBe` 3
            it "sums nested objects" $
                jsonSum (pack "{\"a\":{\"b\":4},\"c\":-1}") `shouldBe` 3
            it "sums mixed objects" $
                jsonSum (pack "{\"a\":[-1,1]}") `shouldBe` 0
            it "sums mixed lists" $
                jsonSum (pack "[-1,{\"a\":1}]") `shouldBe` 0
            it "sums empty lists" $
                jsonSum (pack "[]") `shouldBe` 0
            it "sums empty objects" $
                jsonSum (pack "{}") `shouldBe` 0
        describe "jsonSumFixed" $ do
            it "sums three-member lists" $
                jsonSumFixed (pack "[1,2,3]") `shouldBe` 6
            it "ignores red in nested objects" $
                jsonSumFixed (pack "[1,{\"c\":\"red\",\"b\":2},3]") `shouldBe` 4
            it "ignores red objects" $
                jsonSumFixed (pack "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") `shouldBe` 0
            it "ignores red array elements" $
                jsonSumFixed (pack "[1,\"red\",5]") `shouldBe` 6
