module Y2021.D17Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part17A" $ do
          before (TIO.readFile "dist/resources/2021/day17_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part17A input `shouldBe` 45
        describe "part17B" $ do
          before (TIO.readFile "dist/resources/2021/day17_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part17B input `shouldBe` 112
