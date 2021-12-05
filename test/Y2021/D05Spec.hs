module Y2021.D05Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day5_sample.txt") $ do
          describe "part5A" $ do
            it "solves the example case" $ \input -> do
              part5A input `shouldBe` 5
          describe "part5B" $ do
            it "solves the example case" $ \input -> do
              part5B input `shouldBe` 12
