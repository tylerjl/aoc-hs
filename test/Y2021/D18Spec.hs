module Y2021.D18Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part18A" $ do
          before (TIO.readFile "dist/resources/2021/day18_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part18A input `shouldBe` 4140
        describe "part18B" $ do
          before (TIO.readFile "dist/resources/2021/day18_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part18B input `shouldBe` 3993
