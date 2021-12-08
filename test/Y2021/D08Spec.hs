module Y2021.D08Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day8_sample.txt") $ do
          describe "part8A" $ do
            it "solves the example case" $ \input -> do
              part8A input `shouldBe` 26
          describe "part8B" $ do
            it "solves the example case" $ \input -> do
              part8B input `shouldBe` 61229
