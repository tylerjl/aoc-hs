module Y2018.D03Spec (spec) where

import Y2018

import qualified Data.Set as Set

import Test.Hspec

input = unlines
  [ "#1 @ 1,3: 4x4"
  , "#2 @ 3,1: 4x4"
  , "#3 @ 5,5: 2x2"
  ]

spec :: Spec
spec = parallel $ do
    describe "Day 3" $ do
        describe "overlappedInches" $ do
            it "solves the example case" $
                overlappedInches input `shouldBe` Right 4
        describe "intactInches" $ do
            it "solves the example case" $
                intactInches input `shouldBe` Right [3]
