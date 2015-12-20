module Y2015.D20 (withMinPresents) where

import Data.List (foldl')

withMinPresents :: Int -> Int
withMinPresents n = head houseSums
  where houseSums = [x | x <- [1..]
                       , giftsTo x * 10 >= n]

giftsTo :: Int -> Int
giftsTo n = sum . map (!! (n-1))
        $ [take n $ paddedStep x | x <- [1..n]]

paddedStep :: Int -> [Int]
paddedStep n = map nonzeroToN $ zipStepped [n,n+n..] [1..]
  where nonzeroToN 0 = 0
        nonzeroToN _ = n

zipStepped :: (Eq a, Num a) => [a] -> [a] -> [a]
zipStepped (x:xs) (y:ys) | y == x = x : zipStepped xs ys
                         | otherwise = 0 : zipStepped (x:xs) ys

-- houseWith :: Int -> Int
-- houseWith n = head [x | x <- [1..], factorize x >= (n `div` 10)]
--   where factorize 1 = 1
--         factorize 2 = 3
--         factorize i | isPrime i = i + 1
--                     | otherwise = sum $ i : divisors i

-- Souce: http://stackoverflow.com/a/1480620
-- divisors :: Int -> [Int]
-- divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

-- Source: http://stackoverflow.com/a/4695002
-- isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

-- Source: http://stackoverflow.com/a/3596536
-- primes :: [Integer]
-- primes = sieve [2..]
--   where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
