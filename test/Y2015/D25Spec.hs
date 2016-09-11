module Y2015.D25Spec (spec) where

import Y2015
import Test.Hspec

input =
    "To continue, please consult the code grid in the manual."
        ++ "  Enter the code at row 2, column 1."

spec :: Spec
spec = parallel $
    describe "Day 25" $
        describe "manualCodeFrom" $
          it "returns the correct code" $
            manualCodeFrom input `shouldBe` 31916031
