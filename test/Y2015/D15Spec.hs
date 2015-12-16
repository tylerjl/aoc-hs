module Y2015.D15Spec (spec) where

import Y2015
import Test.Hspec

input = unlines [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
                , "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
                ]

spec :: Spec
spec = parallel $ do
    describe "Day 15" $ do
        describe "cookieScore" $ do
            it "finds the ideal ratio in the example case" $
                cookieScore input `shouldBe` 62842880
        describe "calorieScore" $ do
            it "finds the ideal ratio in the example case with 500 calories" $
                calorieScore input `shouldBe` 57600000
