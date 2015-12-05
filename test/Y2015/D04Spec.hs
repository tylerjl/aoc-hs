module Y2015.D04Spec (spec) where

import Y2015.D04

import Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Day 4" $ do
        describe "mine" $ do
            it "should solve abcdef" $
                mine "abcdef" 5 `shouldBe` 609043
            it "should solve pqrstuv" $
                mine "pqrstuv" 5 `shouldBe` 1048970
