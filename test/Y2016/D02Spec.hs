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
                bathroomCode grid1 (2,2) input `shouldBe` "1985"
            it "finds the solution for the special grid" $
                bathroomCode grid2 (1,3) input `shouldBe` "5DB3"
