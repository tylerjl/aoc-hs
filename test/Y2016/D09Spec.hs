module Y2016.D09Spec (spec) where

import Y2016

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 9" $ do
        describe "inflate" $ do
            it "expands simple codes" $
                inflate "ADVENT" `shouldBe` Right 6
            it "expands one code" $
                inflate "A(1x5)BC" `shouldBe` Right 7
            it "expands double codes" $
                inflate "(3x3)XYZ" `shouldBe` Right 9
            it "expands extended codes" $
                inflate "A(2x2)BCD(2x2)EFG" `shouldBe` Right 11
            it "expands layered codes" $
                inflate "(6x1)(1x3)A" `shouldBe` Right 6
            it "expands big codes" $
                inflate "X(8x2)(3x3)ABCY" `shouldBe` Right 18
