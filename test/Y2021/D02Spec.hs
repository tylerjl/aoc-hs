module Y2021.D02Spec (spec) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Y2021

import Test.Hspec

sample :: Text
sample =
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ] & T.unlines

spec :: Spec
spec = parallel $ do
        describe "part2A" $ do
          it "solves the example case" $
            part2A sample `shouldBe` 150
        describe "part2B" $ do
          it "solves the example case" $
            part2B sample `shouldBe` 900
