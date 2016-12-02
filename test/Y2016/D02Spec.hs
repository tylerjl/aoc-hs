module Y2016.D02Spec (spec) where

import Y2016

import Test.Hspec

input = unlines
    [ "ULL"
    , "RRDDD"
    , "LURDL"
    , "UUUUD"
    ]

spec :: Spec
spec = parallel $ do
    describe "Day 2" $ do
        describe "bathroomCode" $ do
            it "finds the solution for four instructions" $
                bathroomCode input `shouldBe` "1985"
