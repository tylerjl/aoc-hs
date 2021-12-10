module Y2021.D10Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day10_sample.txt") $ do
          describe "part10A" $ do
            it "solves the example case" $ \input -> do
              part10A input `shouldBe` 26397
          describe "part10B" $ do
            it "solves the example case" $ \input -> do
              part10B input `shouldBe` 288957
