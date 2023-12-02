module Y2023.D01Spec (spec) where

import qualified Y2023

import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "Y2023" $ do
          describe "day 1" $ do
            describe "partA" $ do
              before (readFile "./dist/resources/2023/day1-test-a.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.partA input `shouldBe` 142
            describe "partB" $ do
              before (readFile "./dist/resources/2023/day1-test-b.txt") $ do
                it "solves the example case" $ \input -> do
                  Y2023.partB input `shouldBe` 281
