module Y2018.D05Spec (spec) where

import Y2018

import Test.Hspec

input = "dabAcCaCBAcCcaDA"

spec :: Spec
spec = parallel $ do
    describe "Day 5" $ do
        describe "react" $ do
            it "solves the example case" $
                react input `shouldBe` 10
        describe "reactBest" $ do
            it "solves the example case" $
                reactBest input `shouldBe` 4
