module Y2021.D19Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part19A" $ do
          before (TIO.readFile "dist/resources/2021/day19_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part19A input `shouldBe` 79
        describe "part19B" $ do
          before (TIO.readFile "dist/resources/2021/day19_sample.txt") $ do
            it "solves the example case" $ \input -> do
              pending
