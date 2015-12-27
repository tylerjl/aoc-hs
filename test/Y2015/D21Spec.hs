module Y2015.D21Spec (spec) where

import Y2015
import Test.Hspec

player = Combatant { hp    = 8
                   , items = [ item {damage = 5, armor  = 5} ]
                   }

input = unlines [ "Hit Points: 12"
                , "Damage: 7"
                , "Armor: 2"
                ]

spec :: Spec
spec = parallel $
    describe "Day 21" $
        describe "battle" $
          it "correctly simulates a player victory" $
            battle player (toBoss input) `shouldBe` True
