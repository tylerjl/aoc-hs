module Y2015.D15 (cookieScore) where

import Data.List (foldl')

data Ingredient = Ingredient { capacity   :: Int
                             , durability :: Int
                             , flavor     :: Int
                             , texture    :: Int
                             , calories    :: Int
                             } deriving (Show, Eq)

cookieScore :: String -> Int
cookieScore s = maximum [ingredients `recipeSum` x | x <- measurements]
    where ingredients = toIngredients s
          measurements = (length ingredients) `partsOf` 100

recipeSum :: [Ingredient] -> [Int] -> Int
recipeSum i p = product . map (max 0) $ foldl' (zipWith (+)) [0,0,0,0] portions
    where portions = zipWith toScores i p

toScores :: Ingredient -> Int -> [Int]
toScores Ingredient { capacity = c, durability = d, flavor = f, texture = t } =
        flip map [c,d,f,t] . (*)

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
