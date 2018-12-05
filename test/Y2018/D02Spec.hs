module Y2018.D02Spec (spec) where

import Y2018

import Test.Hspec

input = unlines
  [ "abcdef"
  , "bababc"
  , "abbcde"
  , "abcccd"
  , "aabcdd"
  , "abcdee"
  , "ababab"
  ]

spec :: Spec
spec = parallel $ do
    describe "Day 2" $ do
        describe "checksum" $ do
            it "sums correctly" $
                checksum input `shouldBe` 12
