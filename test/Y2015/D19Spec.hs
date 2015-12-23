module Y2015.D19Spec (spec) where

import Y2015
import Test.Hspec

part1 = unlines [ "H => HO"
                , "H => OH"
                , "O => HH"
                , "HOH"
                ]

part2 = unlines $ ["e => H", "e => O"] ++ lines part1

spec :: Spec
spec = parallel $
    describe "Day 19" $ do
        describe "distinctMols" $
            it "finds four distinct modules in the example case" $
                distinctMols part1 `shouldBe` 4
        describe "stepsTo" $ do
            it "finds minimal steps to 'HOH'" $
                molSteps part2 `shouldBe` 3
            it "finds minimal steps to 'HOHOHO'" $
                molSteps (unlines (init (lines part2) ++ ["HOHOHO"]))
                  `shouldBe` 6
