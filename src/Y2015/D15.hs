{-|
Module:      Y2015.D15
Description: Advent of Code Day 15 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 15 set of problems for <adventofcode.com>.
-}

module Y2015.D15 (calorieScore, cookieScore) where

import Data.List (foldl')

data Ingredient = Ingredient { capacity   :: Int
                             , durability :: Int
                             , flavor     :: Int
                             , texture    :: Int
                             , calories   :: Int
                             } deriving (Show, Eq)

-- |Finds a calorie score for a cookie recipe
calorieScore :: String -- ^ Raw string of recipe
             -> Int    -- ^ Calorie score
calorieScore = recipeCombos ((==) 500 . last)

-- |Calculates a cookie score
cookieScore :: String -- ^ Cookie recipe input
            -> Int    -- ^ Calculated cookie score
cookieScore = recipeCombos (const True)

recipeCombos :: ([Int] -> Bool) -> String -> Int
recipeCombos f s = maximum $ map (product . init) $ filter f mixtures
    where ingredients  = toIngredients s
          measurements = length ingredients `partsOf` 100
          mixtures     = [ingredients `recipeSum` x | x <- measurements]

recipeSum :: [Ingredient] -> [Int] -> [Int]
recipeSum i p = map (max 0) $ foldl' (zipWith (+)) [0,0,0,0,0] portions
    where portions = zipWith toScores i p

toScores :: Ingredient -> Int -> [Int]
toScores Ingredient { capacity = c, durability = d, flavor = f, texture = t, calories = ca } =
        flip map [c,d,f,t,ca] . (*)

partsOf :: Int -> Int -> [[Int]]
partsOf n total | n > 1     = [x : y | x <- [1..(total-1)], y <- (n-1) `partsOf` (total-x)]
                | otherwise = [[total]]

toIngredients :: String -> [Ingredient]
toIngredients = map (toIngredient . words) . lines . filter (/= ',')

toIngredient :: [String] -> Ingredient
toIngredient [_,_,c,_,d,_,f,_,t,_,ca] =
    Ingredient { capacity   = read c
               , durability = read d
               , flavor     = read f
               , texture    = read t
               , calories   = read ca
               }
toIngredient _ =
    Ingredient { capacity   = 0
               , durability = 0
               , flavor     = 0
               , texture    = 0
               , calories   = 0
               }
