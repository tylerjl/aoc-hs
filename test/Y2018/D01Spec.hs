module Y2018.D01Spec (spec) where

import Y2018

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 1" $ do
        describe "frequency" $ do
            it "sums the first example" $
                frequency (unlines ["+1", "-2", "+3", "+1"]) `shouldBe` Just 3
            it "sums the second example" $
                frequency (unlines ["+1", "+1", "+1"]) `shouldBe` Just 3
            it "sums the third example" $
                frequency (unlines ["+1", "+1", "-2"]) `shouldBe` Just 0
            it "sums the fourth example" $
                frequency (unlines ["-1", "-2", "-3"]) `shouldBe` Just (-6)
        describe "twiceFrequency" $ do
            it "finds the first example" $
                twiceFrequency (unlines ["+1", "-2", "+3", "+1"]) `shouldBe` Just 2
            it "finds the second example" $
                twiceFrequency (unlines ["+1", "-1"]) `shouldBe` Just 0
            it "finds the third example" $
                twiceFrequency (unlines ["+3", "+3", "+4", "-2", "-4"]) `shouldBe` Just 10
            it "finds the fourth example" $
                twiceFrequency (unlines ["-6", "+3", "+8", "+5", "-6"]) `shouldBe` Just 5
            it "finds the fifth example" $
                twiceFrequency (unlines ["+7", "+7", "-2", "-7", "-4"]) `shouldBe` Just 14
