module Y2015.D20Spec (spec) where

import Y2015
import Test.Hspec

spec :: Spec
spec = parallel $
    describe "Day 20" $
        describe "withMinPresents" $
            it "finds 70 presents at house four" $
                withMinPresents 70 `shouldBe` 4
