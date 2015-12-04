module Y2015.D02Spec (spec) where

import Y2015.D02

import Test.Hspec

spec :: Spec
spec =
    describe "Day 2" $ do
        describe "surfaceArea" $ do
            it "calculates small presents" $
                pendingWith "need to figure out Maybe wrapper"
                -- surfaceArea (parsePresents "2x3x4") `shouldBe` 58
            it "calculates large presents" $
                pendingWith "need to figure out Maybe wrapper"
                -- surfaceArea (parsePresents "1x1x10") `shouldBe` 43
