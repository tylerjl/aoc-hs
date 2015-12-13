module Y2015.D02Spec (spec) where

import Y2015

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 2" $ do
        describe "parsePresents" $ do
            it "parses small presents" $
                parsePresents "2x3x4" `shouldBe` Just [(Present 2 3 4)]
            it "parses large presents" $
                parsePresents "1x1x10" `shouldBe` Just [(Present 1 1 10)]
        describe "surfaceArea" $ do
            it "calculates small presents" $
                surfaceArea [(Present 2 3 4)] `shouldBe` 58
            it "calculates large presents" $
                surfaceArea [(Present 1 1 10)] `shouldBe` 43
        describe "ribbonLength" $ do
            it "calculates small presents" $
                ribbonLength [(Present 2 3 4)] `shouldBe` 34
            it "calculates large presents" $
                ribbonLength [(Present 1 1 10)] `shouldBe` 14
