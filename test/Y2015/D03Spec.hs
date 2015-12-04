module Y2015.D03Spec (spec) where

import Y2015.D03

import Test.Hspec

spec :: Spec
spec =
    describe "Day 3" $ do
        describe "solve" $ do
            it "should deliver to 2 houses" $
                solve ">" `shouldBe` 2
            it "should deliver to 4 houses in a square" $
                solve "^>v<" `shouldBe` 4
            it "should deliver many presents to 2 houses" $
                solve "^v^v^v^v^v" `shouldBe` 2

        describe "roboSolve" $ do
            it "should deliver to 3 houses" $
                roboSolve "^v" `shouldBe` 3
            it "should deliver to 3 houses and return to origin" $
                roboSolve "^>v<" `shouldBe` 3
            it "should deliver 11 presents" $
                roboSolve "^v^v^v^v^v" `shouldBe` 11
