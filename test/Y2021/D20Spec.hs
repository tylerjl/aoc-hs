module Y2021.D20Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

spec :: Spec
spec = parallel $ do
        describe "part20A" $ do
          before (TIO.readFile "dist/resources/2021/day20_sample.txt") $ do
            it "solves the example case" $ \input -> do
              pending
        describe "part20B" $ do
          before (TIO.readFile "dist/resources/2021/day20_sample.txt") $ do
            it "solves the example case" $ \input -> do
              pending
