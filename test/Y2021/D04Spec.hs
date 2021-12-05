module Y2021.D04Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        before (TIO.readFile "dist/resources/2021/day4_sample.txt") $ do
          describe "part4A" $ do
            it "solves the example case" $ \input -> do
              part4A input `shouldBe` 4512
          describe "part4B" $ do
            it "solves the example case" $ \input -> do
              part4B input `shouldBe` 1924
