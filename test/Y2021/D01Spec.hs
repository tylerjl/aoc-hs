module Y2021.D01Spec (spec) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Y2021

import Test.Hspec

sample :: Text
sample = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  & map (into @Text . show) & T.unlines

spec :: Spec
spec = parallel $ do
        describe "partA" $ do
          it "solves the example case" $
            partA sample `shouldBe` 7
        describe "partB" $ do
          it "solves the example case" $
            partB sample `shouldBe` 5
