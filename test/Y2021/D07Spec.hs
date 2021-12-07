module Y2021.D07Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day7_sample.txt") $ do
          describe "part7A" $ do
            it "solves the example case" $ \input -> do
              part7A input `shouldBe` 37
          describe "part7B" $ do
            it "solves the example case" $ \input -> do
              part7B input `shouldBe` 168
