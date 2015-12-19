module Y2015.D18Spec (spec) where

import Y2015
import Test.Hspec

input = ".#.#.#...##.#....#..#...#.#..#####.."

spec :: Spec
spec = parallel $ do
    describe "Day 18" $ do
        describe "animateLights" $ do
            it "lights four lights after four steps" $
                animateLights input 4 `shouldBe` 4
        describe "animateStuckLights" $ do
            it "lights four lights after four steps" $
                animateStuckLights input 5 `shouldBe` 17
