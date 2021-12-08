module Y2021.D01Spec (spec) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Witch
import Y2021

import Test.Hspec

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day1_sample.txt") $ do
          describe "part1A" $ do
            it "solves the example case" $ \input -> do
              part1A input `shouldBe` 7
          describe "part1AZip" $ do
            it "solves the example case" $ \input -> do
              part1AZip input `shouldBe` 7
          describe "part1ARecur" $ do
            it "solves the example case" $ \input -> do
              part1ARecur input `shouldBe` 7
          describe "part1B" $ do
            it "solves the example case" $ \input -> do
              part1B input `shouldBe` 5
          describe "part1BZip" $ do
            it "solves the example case" $ \input -> do
              part1BZip input `shouldBe` 5
