module Y2021.D12Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part12A" $ do
          before (TIO.readFile "dist/resources/2021/day12_small.txt") $ do
            it "solves the small case" $ \input -> do
              part12A input `shouldBe` 10
          before (TIO.readFile "dist/resources/2021/day12_medium.txt") $ do
            it "solves the medium case" $ \input -> do
              part12A input `shouldBe` 19
          before (TIO.readFile "dist/resources/2021/day12_large.txt") $ do
            it "solves the large case" $ \input -> do
              part12A input `shouldBe` 226
        describe "part12B" $ do
          before (TIO.readFile "dist/resources/2021/day12_small.txt") $ do
            it "solves the small case" $ \input -> do
              part12B input `shouldBe` 36
          before (TIO.readFile "dist/resources/2021/day12_medium.txt") $ do
            it "solves the medium case" $ \input -> do
              part12B input `shouldBe` 103
          before (TIO.readFile "dist/resources/2021/day12_large.txt") $ do
            it "solves the large case" $ \input -> do
              part12B input `shouldBe` 3509
