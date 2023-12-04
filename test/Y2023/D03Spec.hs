module Y2023.D03Spec (spec) where

import qualified Y2023

import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "Y2023" $ do
          describe "day 3" $ do
            describe "part3A" $ do
              before (TIO.readFile "./dist/resources/2023/day3-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part3A input `shouldBe` 4361
            describe "part3B" $ do
              before (TIO.readFile "./dist/resources/2023/day3-test.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.part3B input `shouldBe` 467835
