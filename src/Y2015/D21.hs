{-|
Module:      Y2015.D21
Description: Advent of Code Day 21 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 21 set of problems for <adventofcode.com>.
-}

module Y2015.D21
  ( battle
  , cheapestVictory
  , highestLoss
  , toBoss
  , mkTestCombatant
  )
where

import Control.Monad (replicateM)
import Data.List     (maximumBy, minimumBy)
import Data.Ord      (comparing)

data Item = Item { cost   :: Int
                 , armor  :: Int
                 , damage :: Int
                 } deriving (Show)

data Combatant = Combatant { hp    :: Int
                           , items :: [Item]
                           } deriving (Show)

-- |Utility function to generate a cheap test 'Combatant'.
mkTestCombatant :: Combatant -- ^ Low-complexity 'Combatant'.
mkTestCombatant =
    Combatant
        { hp    = 8
        , items = [ item {damage = 5, armor  = 5} ]
        }

-- |Finds the worst possible loss given combatant stats.
highestLoss :: String -- ^ Raw combatant stats.
            -> Int    -- ^ Highest possible loss as an Int
highestLoss = battleCostBy maximumBy (not . fst)

-- |Finds the cheapest possible victory given combatant stats.
cheapestVictory :: String -- ^ Raw combatant stats.
                -> Int    -- ^ Cheapest possible victory as an Int
cheapestVictory = battleCostBy minimumBy fst

battleCostBy :: Ord a1
             => (((a2, a1) -> (a2, a1) -> Ordering) -> [(Bool, Int)] -> (a, c))
             -> ((Bool, Int) -> Bool)
             -> String
             -> c
battleCostBy f g = snd . f (comparing snd) . filter g
                 . flip map loadouts . battle . toBoss

loadouts :: [Combatant]
loadouts = map (equip player) [ w:a:rs | w  <- weapons
                                       , a  <- armory
                                       , rs <- replicateM 2 rings]

player :: Combatant
player = Combatant { hp = 100, items = [] }

-- |Parses a string into a 'Combatant'
toBoss :: String    -- ^ Raw combatant stats
       -> Combatant -- ^ Resultant combatant record
toBoss s = Combatant { hp = hp', items = i }
  where input = lines s
        hp'    = read $ last $ words $ head input
        i      = map (toI . words) $ tail input
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
equip c@Combatant {items = is} i = c {items = i ++ is}

attr :: (Item -> Int) -> Combatant -> Int
attr f = sum . map f . items

-- |Simulates the outcome of two 'Combatant's dueling.
battle :: Combatant   -- ^ Player 1
       -> Combatant   -- ^ Player 2
       -> (Bool, Int) -- ^ Tuple containing whether first player won, and
                      -- ^ at what price.
battle b p | (p `hits` b) <= (b `hits` p) = (True,  price)
           | otherwise                    = (False, price)
           where price = attr cost p

hits :: Combatant -> Combatant -> Int
hits a d = ceiling ((fromIntegral (hp d) / fromIntegral minDmg) :: Double)
  where minDmg = max 1 (attr damage a - attr armor d)
