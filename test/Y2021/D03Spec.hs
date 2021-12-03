module Y2021.D03Spec (spec) where

import Witch
import Y2021

import Test.Hspec
import qualified Data.Text as T
import Data.Text (Text)
import Data.Function ((&))

sample :: Text
sample =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ] & T.unlines

spec :: Spec
spec = parallel $ do
        describe "part3A" $ do
          it "solves the example case" $
            part3A sample `shouldBe` 198
        describe "part3B" $ do
          it "solves the example case" $
            part3B sample `shouldBe` 230
