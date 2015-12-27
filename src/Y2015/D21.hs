module Y2015.D21
  ( Combatant(..)
  , Item(..)
  , battle
  , cheapestVictory
  , item
  , toBoss
  )
where

import Control.Monad (replicateM)
import Data.Maybe    (catMaybes)
import Safe          (minimumMay)

data Item = Item { cost   :: Int
                 , armor  :: Int
                 , damage :: Int
                 } deriving (Show)

data Combatant = Combatant { hp    :: Int
                           , items :: [Item]
                           } deriving (Show)

cheapestVictory :: String -> Maybe Int
cheapestVictory = minimumMay . catMaybes . flip map tests . battle . toBoss
  where tests = map (equip player) loadouts

loadouts :: [[Item]]
loadouts = [ w:a:rs | w  <- weapons
                    , a  <- armory
                    , rs <- replicateM 2 rings]

player :: Combatant
player = Combatant { hp = 100, items = [] }

toBoss :: String -> Combatant
toBoss s = Combatant { hp = hp, items = i }
  where input = lines s
        hp    = read $ last $ words $ head input
        i     = map (toI . words) $ tail input
        toI ["Damage:",d] = item { damage = read d }
        toI ["Armor:", a] = item { armor  = read a }
        toI _             = item

item :: Item
item = Item {cost = 0, damage = 0, armor = 0}

weapons :: [Item]
weapons = [ item {cost = 8,  damage = 4}
          , item {cost = 10, damage = 5}
          , item {cost = 25, damage = 6}
          , item {cost = 40, damage = 7}
          , item {cost = 74, damage = 8}
          ]

armory :: [Item]
armory = [ item {cost = 13,  armor = 1}
         , item {cost = 31,  armor = 2}
         , item {cost = 53,  armor = 3}
         , item {cost = 75,  armor = 4}
         , item {cost = 102, armor = 5}
         , item -- Dummy piece since armor is optional
         ]

rings :: [Item]
rings = [ item {cost = 25,  damage = 1}
        , item {cost = 50,  damage = 2}
        , item {cost = 100, damage = 3}
        , item {cost = 20,   armor = 1}
        , item {cost = 40,   armor = 2}
        , item {cost = 80,   armor = 3}
        , item -- Dummy piece since rings are optional
        ]

equip :: Combatant -> [Item] -> Combatant
equip c@(Combatant {items = is}) i = c {items = i ++ is}

attr :: (Item -> Int) -> Combatant -> Int
attr f = sum . map f . items

battle :: Combatant -> Combatant -> Maybe Int
battle b p | (p `hits` b) <= (b `hits` p) = Just $ attr cost p
           | otherwise                    = Nothing

hits :: Combatant -> Combatant -> Int
hits a d = ceiling $ fromIntegral (hp d) / fromIntegral minDmg
  where minDmg = max 1 (attr damage a - attr armor d)
