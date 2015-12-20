module Y2015.D19Spec (spec) where

import Y2015
import Test.Hspec

input = unlines [ "H => HO"
                , "H => OH"
                , "O => HH"
                , "HOH"
                ]

spec :: Spec
spec = parallel $
    describe "Day 19" $
        describe "distinctMols" $
            it "finds four distinct modules in the example case" $
                distinctMols input `shouldBe` 4
