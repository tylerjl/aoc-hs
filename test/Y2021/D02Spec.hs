module Y2021.D02Spec (spec) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Y2021

import Test.Hspec

spec :: Spec
spec = parallel $ do
        describe "part2A" $ do
          it "solves the example case" $
            part2A d2sample `shouldBe` 150
        describe "part2B" $ do
          it "solves the example case" $
            part2B d2sample `shouldBe` 900
