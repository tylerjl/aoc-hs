module Y2015.D06Spec (spec) where

import Y2015.D06

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 6" $ do
        describe "testA" $ do
            it "turns on every light" $
                testA (On (Range (0,0) (999,999))) `shouldBe` 1000000
            it "toggle the first line of 1000 lights" $
                testA (Toggle (Range (0,0) (999,0))) `shouldBe` 1000
            it "turns on the middle four lights" $
                testA (On (Range (499,499) (500,500))) `shouldBe` 4
        describe "testB" $ do
            it "increases total brightness by 1" $
                testB (On (Range (0,0) (0,0))) `shouldBe` 1
            it "toggles whole-grid brightness" $
                testB (Toggle (Range (0,0) (999,999))) `shouldBe` 2000000
