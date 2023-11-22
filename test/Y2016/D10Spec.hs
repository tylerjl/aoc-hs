module Y2016.D10Spec (spec) where

import Y2016

import Data.Text
import qualified Data.Text.IO as TIO
import Test.Hspec

getSample :: FilePath -> IO Text
getSample n = TIO.readFile $ "dist/resources/2016/day" <> n <> "-test"

spec :: Spec
spec = parallel $ do
    describe "Day 10" $ do
        describe "findBot" $ do
            it "solves the example" $ do
                input <- getSample "10"
                findBot [5,2] input `shouldBe` [2]
        describe "findOutputs" $ do
            it "solves the example" $ do
                input <- getSample "10"
                findOutputs [0,1,2] input `shouldBe` 30
