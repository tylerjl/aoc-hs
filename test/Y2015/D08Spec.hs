module Y2015.D08Spec (spec) where

import Y2015.D08

import Test.Hspec

input = unlines [ "\"\""
                , "\"abc\""
                , "\"aaa\\\"aaa\""
                , "\"\\x27\""
                ]

spec :: Spec
spec = parallel $ do
    describe "Day 8" $ do
        describe "difference" $ do
            it "calculates the difference in the sample text" $
                difference input `shouldBe` 12
        describe "encode" $ do
            it "calcuates the encoded difference in the sample text" $
                encoded input `shouldBe` 19
