module Y2021.D15Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part15A" $ do
          before (TIO.readFile "dist/resources/2021/day15_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part15A input `shouldBe` 40
        describe "part15B" $ do
          before (TIO.readFile "dist/resources/2021/day15_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part15B input `shouldBe` 315
