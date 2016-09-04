module Y2015.D24Spec (spec) where

import Y2015
import Test.Hspec

input = unlines $ map show $ [1..5] ++ [7..11]

spec :: Spec
spec = parallel $
    describe "Day 24" $
        describe "idealEntanglement" $
          it "finds the ideal example arrangement entanglement" $
            idealEntanglement input `shouldBe` 99
