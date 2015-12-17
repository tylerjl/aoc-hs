module Y2015.D17Spec (spec) where

import Y2015
import Test.Hspec

input = unlines [ "20", "15", "10", "5", "5" ]

spec :: Spec
spec = parallel $ do
    describe "Day 17" $ do
        describe "filledAmong" $ do
            it "distributes 25 liters among four combinations" $
                25 `filledAmong` input `shouldBe` 4
        describe "minFilledAmong" $ do
            it "distributes 25 liters among 3 combinations of 2 containers" $
                25 `minFilledAmong` input `shouldBe` 3
