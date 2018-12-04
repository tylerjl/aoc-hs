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
