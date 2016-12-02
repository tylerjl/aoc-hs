module Y2016.D01Spec (spec) where

import Y2016

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 1" $ do
        describe "blockDistance" $ do
            it "counts a simple case" $
                blockDistance "R2, L3" `shouldBe` 5
            it "counts looping routes" $
                blockDistance "R2, R2, R2" `shouldBe` 2
            it "counts longer routes" $
                blockDistance "R5, L5, R5, R3" `shouldBe` 12
        describe "visitedTwice" $ do
            it "finds easter bunny HQ" $
                visitedTwice "R8, R4, R4, R8" `shouldBe` Just 4
