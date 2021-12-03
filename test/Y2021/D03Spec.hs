module Y2021.D03Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import Data.Text (Text)
import Data.Function ((&))

spec :: Spec
spec = parallel $ do
        describe "part3A" $ do
          it "solves the example case" $
            part3A sample3 `shouldBe` 198
        describe "part3B" $ do
          it "solves the example case" $
            part3B sample3 `shouldBe` 230
