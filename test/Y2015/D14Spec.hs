module Y2015.D14Spec (spec) where

import Y2015
import Test.Hspec

input = unlines [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
                , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
                ]

spec :: Spec
spec = parallel $ do
    describe "Day 14" $ do
        describe "distanceRace" $ do
            it "returns Comet's winning distance after 1000 seconds" $
                distanceRace input 1000 `shouldBe` 1120
        describe "leadingRace" $ do
            it "returns Dancer's points 1000 seconds" $
                leadingRace input 1000 `shouldBe` 689
