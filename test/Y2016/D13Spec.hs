module Y2016.D13Spec (spec) where

import Y2016

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 13" $ do
        describe "officePath" $ do
            it "solves the example" $ do
                officePath 10 (7, 4) `shouldBe` Just 11
