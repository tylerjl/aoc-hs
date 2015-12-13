module Y2015.D01Spec (spec) where

import Y2015

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 1" $ do
        describe "level" $ do
            it "navigates to zero correctly" $
                level "(())" `shouldBe` 0
            it "navigates straight up" $
                level "(((" `shouldBe` 3
            it "navigates to 3" $
                level "(()(()(" `shouldBe` 3
            it "navigates to -1 indirectly" $
                level "())" `shouldBe` (-1)
            it "navigates to -1 directly" $
                level "))(" `shouldBe` (-1)
            it "navigates straight down" $
                level ")))" `shouldBe` (-3)
            it "navigates to -3 indirectly" $
                level ")())())" `shouldBe` (-3)
        describe "basement" $ do
            it "enters the basement immediately" $
                basement ")" `shouldBe` Just 1
            it "moves around for a while first" $
                basement "()())" `shouldBe` Just 5
