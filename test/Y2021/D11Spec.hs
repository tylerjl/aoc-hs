module Y2021.D11Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day11_sample.txt") $ do
          describe "part11A" $ do
            it "solves the example case" $ \input -> do
              part11A input `shouldBe` 1656
          describe "part11B" $ do
            it "solves the example case" $ \input -> do
              part11B input `shouldBe` 195
