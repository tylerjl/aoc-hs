{-|
Module:      Y2015.D20
Description: Advent of Code Day 20 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 20 set of problems for <adventofcode.com>.

Original credit for most of this due to aepsilon:
<https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/cy5brqe>
-}

module Y2015.D20 (withMinPresents, withMinPresents2) where

import           Data.List   (group)
import           Data.Set    (Set)
import qualified Data.Set as Set

-- |Finds lowest house number that receives as many as the given presents
withMinPresents :: Int -- ^ Minimum number of presents house should receive
                -> Int -- ^ Position of house
withMinPresents n = head $ filter ((>=n `divCeil` 10) . divisorSum) [1..]

divCeil :: Int -> Int -> Int
n `divCeil` d = (n-1) `div` d + 1

divisorSum :: Int -> Int
divisorSum = sum . divisorList

divisorList :: Int -> [Int]
divisorList = map product . mapM (scanl (*) 1) . group . factorize

factorize :: Int -> [Int]
factorize 1                = []
factorize n | null factors = [n]
            | otherwise    = p : factorize q
  where factors = [ (p',q') | p' <- takeWhile (<= intsqrt n) primes
                          , let (q',r) = quotRem n p'
                          , r == 0 ]
        p = fst $ head factors
        q = snd $ head factors

intsqrt :: Int -> Int
intsqrt i = floor ((sqrt $ fromIntegral i) :: Double)

primes :: [Int]
primes = 2 : 3 : [ p | p <- [5,7..]
                     , and [p `rem` d /= 0 | d <- takeWhile (<= intsqrt p) primes]
                 ]

-- |Finds lowest house number that receives as many as the given presents
-- |given the special delivery case.
withMinPresents2 :: Int -- ^ Minimum number of presents house should receive
                 -> Int -- ^ Position of house
withMinPresents2 n = head $ filter ((>=n `divCeil` 11)
                          . divisorSumLimit 50) [1..]

divisorSumLimit :: Int -> Int -> Int
divisorSumLimit limit n = sum . snd . Set.split ((n-1)`div`limit) . divisors $ n

divisors :: Int -> Set Int
divisors = Set.fromList . divisorList
