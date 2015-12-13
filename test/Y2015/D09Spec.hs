module Y2015.D09Spec (spec) where

import Y2015
import Y2015.Util

import Test.Hspec

input = unlines [ "London to Dublin = 464"
                , "London to Belfast = 518"
                , "Dublin to Belfast = 141"
                ]

routes = case regularParse routeParser input of
              Right rs -> rs
              Left _   -> []

spec :: Spec
spec = parallel $ do
    describe "Day 9" $ do
        describe "shortestRoute" $ do
            it "finds the shortest path in the example case" $
                shortestRoute routes `shouldBe` Just 605
        describe "longestRoute" $ do
            it "finds the longest path in the example case" $
                longestRoute routes `shouldBe` Just 982
