module Y2023.D04Spec (spec) where

import qualified Y2023

import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "Y2023" $ do
          describe "day 4" $ do
            describe "part4A" $ do
              before (TIO.readFile "./dist/resources/2023/day4-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part4A input `shouldBe` 13
            describe "part4B" $ do
              before (TIO.readFile "./dist/resources/2023/day4-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part4B input `shouldBe` 30
