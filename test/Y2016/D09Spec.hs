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

        describe "decompression" $ do
            it "expands singly-tested" $
                nestedInflate "(3x3)XYZ" `shouldBe` 9
            it "expands double-nested" $
                nestedInflate "X(8x2)(3x3)ABCY" `shouldBe` 20
            it "expands nested single characters" $
                nestedInflate "(27x12)(20x12)(13x14)(7x10)(1x12)A" `shouldBe` 241920
            it "expands big codes" $
                nestedInflate "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" `shouldBe` 445
