module Y2015.D14Spec (spec) where

import Y2015
import Test.Hspec

input = unlines [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
                , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
                ]

spec :: Spec
spec = parallel $ do
    describe "Day 14" $ do
        describe "reinRace" $ do
            it "returns comet's winning distance after 1000 seconds" $
                reinRace input 1000 `shouldBe` 1120
