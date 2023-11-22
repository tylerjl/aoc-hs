module Y2016.D12Spec (spec) where

import Y2016

import Data.Text
import qualified Data.Text.IO as TIO
import Test.Hspec

getSample :: FilePath -> IO Text
getSample n = TIO.readFile $ "dist/resources/2016/day" <> n <> "-test"

spec :: Spec
spec = parallel $ do
    describe "Day 12" $ do
        describe "assembunnyRegister" $ do
            it "solves the example" $ do
                input <- getSample "12"
                assembunnyRegister 'a' input `shouldBe` 42
