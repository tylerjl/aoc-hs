module Y2021.D13Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part13A" $ do
          before (TIO.readFile "dist/resources/2021/day13_sample.txt") $ do
            it "solves the small case" $ \input -> do
              part13A input `shouldBe` 17
