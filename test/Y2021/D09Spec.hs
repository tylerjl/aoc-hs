module Y2021.D09Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day9_sample.txt") $ do
          describe "part9A" $ do
            it "solves the example case" $ \input -> do
              part9A input `shouldBe` 15
          describe "part9B" $ do
            it "solves the example case" $ \input -> do
              part9B input `shouldBe` 1134
