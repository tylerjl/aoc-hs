module Y2021.D06Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day6_sample.txt") $ do
          describe "part6A" $ do
            it "solves the example case" $ \input -> do
              part6A input `shouldBe` 5934
            it "solves the example case using vectors" $ \input -> do
              part6AMV input `shouldBe` 5934
          describe "part6B" $ do
            it "solves the example case" $ \input -> do
              part6B input `shouldBe` 26984457539
            it "solves the example case using vectors" $ \input -> do
              part6BMV input `shouldBe` 26984457539
