module Y2015.D03Spec (spec) where

import Y2015

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 3" $ do
        describe "santaRun" $ do
            it "should deliver to 2 houses" $
                santaRun ">" `shouldBe` 2
            it "should deliver to 4 houses in a square" $
                santaRun "^>v<" `shouldBe` 4
            it "should deliver many presents to 2 houses" $
                santaRun "^v^v^v^v^v" `shouldBe` 2

        describe "roboSolve" $ do
            it "should deliver to 3 houses" $
                roboRun "^v" `shouldBe` 3
            it "should deliver to 3 houses and return to origin" $
                roboRun "^>v<" `shouldBe` 3
            it "should deliver 11 presents" $
                roboRun "^v^v^v^v^v" `shouldBe` 11
