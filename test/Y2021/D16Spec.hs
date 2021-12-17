module Y2021.D16Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part16A" $ do
          before (TIO.readFile "dist/resources/2021/day16.txt") $ do
            it "solves the puzzle input" $ \input -> do
              part16A input `shouldBe` 906
        describe "part16B" $ do
          before (TIO.readFile "dist/resources/2021/day16.txt") $ do
            it "solves the puzzle input" $ \input -> do
              part16B input `shouldBe` 819324480368
