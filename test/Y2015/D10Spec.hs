module Y2015.D10Spec (spec) where

import Y2015.D10
import Test.Hspec

input = solve "1"

spec :: Spec
spec = parallel $ do
    describe "Day 10" $ do
        describe "solve" $ do
            it "iterates once correctly" $
                input 1 `shouldBe` "11"
            it "iterates twice correctly" $
                input 2 `shouldBe` "21"
            it "iterates thrice correctly" $
                input 3 `shouldBe` "1211"
            it "iterates four times correctly" $
                input 4 `shouldBe` "111221"
            it "iterates five times correctly" $
                input 5 `shouldBe` "312211"
