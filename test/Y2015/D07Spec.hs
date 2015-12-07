module Y2015.D07Spec (spec) where

import Y2015.D07
import Y2015.Util

import Test.Hspec

input = unlines [ "123 -> x"
                , "456 -> y"
                , "x AND y -> d"
                , "x OR y -> e"
                , "x LSHIFT 2 -> f"
                , "y RSHIFT 2 -> g"
                , "NOT x -> h"
                , "NOT y -> i"
                ]

instructions = case regularParse circuitParser input of
                    Right i  -> i
                    Left _   -> []

spec :: Spec
spec = parallel $ do
    describe "Day 7" $ do
        describe "voltageOn" $ do
            it "calculates test case d correctly" $
                solve "d" instructions `shouldBe` 72
            it "calculates test case e correctly" $
                solve "e" instructions `shouldBe` 507
            it "calculates test case f correctly" $
                solve "f" instructions `shouldBe` 492
            it "calculates test case g correctly" $
                solve "g" instructions `shouldBe` 114
            it "calculates test case h correctly" $
                solve "h" instructions `shouldBe` 65412
            it "calculates test case i correctly" $
                solve "i" instructions `shouldBe` 65079
            it "calculates test case x correctly" $
                solve "x" instructions `shouldBe` 123
            it "calculates test case y correctly" $
                solve "y" instructions `shouldBe` 456
