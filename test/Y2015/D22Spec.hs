module Y2015.D22Spec (spec) where

import Y2015
import Test.Hspec
import Data.Map.Strict as M

input = unlines [ "inc a"
                , "jio a, +2"
                , "tpl a"
                , "inc a"
                ]

spec :: Spec
spec = parallel $
    describe "Day 22" $
        describe "exInstructions" $
          it "calcuates the example instruction set" $
            exInstructions input `shouldBe` M.singleton 'a' 2
