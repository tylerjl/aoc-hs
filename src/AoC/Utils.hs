module AoC.Utils
  ( (~~)
  ) where

-- |Messing around with a custom operator for ranges that can handle both up-to
-- and down-to.
infixl 5 ~~
(~~) :: Int -> Int -> [Int]
a ~~ b | a <= b    = [a .. b]
       | otherwise = reverse $ b ~~ a
