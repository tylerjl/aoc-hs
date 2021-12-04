module Y2021.D01Spec (spec) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Y2021

import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "partA" $ do
          it "solves the example case" $
            partA d1sample `shouldBe` 7
        describe "partAZip" $ do
          it "solves the example case" $
            partAZip d1sample `shouldBe` 7
        describe "partARecur" $ do
          it "solves the example case" $
            partARecur d1sample `shouldBe` 7
        describe "partB" $ do
          it "solves the example case" $
            partB d1sample `shouldBe` 5
        describe "partBZip" $ do
          it "solves the example case" $
            partBZip d1sample `shouldBe` 5
