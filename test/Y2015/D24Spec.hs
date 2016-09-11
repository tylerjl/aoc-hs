module Y2015.D24Spec (spec) where

import Y2015
import Test.Hspec

input = unlines $ map show $ [1..5] ++ [7..11]

spec :: Spec
spec = parallel $
    describe "Day 24" $
        describe "idealEntanglement" $ do
          it "finds the ideal example arrangement entanglement among 3 compartments" $
            idealEntanglement 3 input `shouldBe` 99
          it "finds the ideal example arrangement entanglement among 4 compartments" $
            idealEntanglement 4 input `shouldBe` 44
