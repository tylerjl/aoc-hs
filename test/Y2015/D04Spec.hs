module Y2015.D04Spec (spec) where

{-# LANGUAGE OverloadedStrings #-}

import Y2015
import Test.Hspec
import Data.ByteString.Char8 (pack)

spec :: Spec
spec = parallel $ do
    describe "Day 4" $ do
        describe "crack" $ do
            it "should solve abcdef" $
                crack (pack "abcdef") 5 `shouldBe` 609043
            it "should solve pqrstuv" $
                crack (pack "pqrstuv") 5 `shouldBe` 1048970
