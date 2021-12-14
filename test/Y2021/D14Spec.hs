module Y2021.D14Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part14A" $ do
          before (TIO.readFile "dist/resources/2021/day14_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part14A input `shouldBe` 1588
        describe "part14B" $ do
          before (TIO.readFile "dist/resources/2021/day14_sample.txt") $ do
            it "solves the example case" $ \input -> do
              part14B input `shouldBe` 2188189693529
