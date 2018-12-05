module Y2018.D02Spec (spec) where

import Y2018

import Test.Hspec

part1 = unlines
  [ "abcdef"
  , "bababc"
  , "abbcde"
  , "abcccd"
  , "aabcdd"
  , "abcdee"
  , "ababab"
  ]

part2 = unlines
  [ "abcde"
  , "fghij"
  , "klmno"
  , "pqrst"
  , "fguij"
  , "axcye"
  , "wvxyz"
  ]

spec :: Spec
spec = parallel $ do
    describe "Day 2" $ do
        describe "checksum" $ do
            it "sums correctly" $
                checksum part1 `shouldBe` 12
        describe "boxID" $ do
            it "finds the correct box" $
                boxID part2 `shouldBe` Just "fgij"
