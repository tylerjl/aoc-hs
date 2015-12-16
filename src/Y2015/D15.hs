module Y2015.D15 (calorieScore, cookieScore) where

import Data.List (foldl')

data Ingredient = Ingredient { capacity   :: Int
                             , durability :: Int
                             , flavor     :: Int
                             , texture    :: Int
                             , calories    :: Int
                             } deriving (Show, Eq)

calorieScore :: String -> Int
calorieScore = maximum . map (product . init) . filter ((==) 500 . last) . recipeCombos

cookieScore :: String -> Int
cookieScore = maximum . map (product . init) . recipeCombos

recipeCombos :: String -> [[Int]]
recipeCombos s = [ingredients `recipeSum` x | x <- measurements]
    where ingredients = toIngredients s
          measurements = length ingredients `partsOf` 100

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
