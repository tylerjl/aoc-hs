module Y2023.D02Spec (spec) where

import qualified Y2023

import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "Y2023" $ do
          describe "day 2" $ do
            describe "part2A" $ do
              before (TIO.readFile "./dist/resources/2023/day2-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part2A input `shouldBe` 8
            describe "part2B" $ do
              before (TIO.readFile "./dist/resources/2023/day2-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part2B input `shouldBe` 2286
