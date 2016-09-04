module Y2015.D24 (idealEntanglement) where

import Data.List (permutations)
import Data.Maybe (catMaybes)

test = [1..5] ++ [7..11]

idealEntanglement :: String -> [Int]
idealEntanglement = map read . lines

-- |Return a list of tuples split at the index at which the
-- |lists are equal.
splitEven :: (Eq a, Num a) => [a] -> [([a], [a])]
splitEven [] = []
splitEven as = catMaybes $ go 1
  where go n | n == length as = []
             | otherwise =
               let
                 r = case splitAt n as of
                          (h, t) ->
                            if sum h == sum t
                              then [Just (h, t)]
                              else [Nothing]
               in
                 r ++ go (n+1)
