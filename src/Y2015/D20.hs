module Y2015.D20 (withMinPresents, withMinPresents2) where

-- Original credit for most of this due to aepsilon:
-- https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/cy5brqe

import           Data.List   (group, null)
import           Data.Set    (Set)
import qualified Data.Set as Set

withMinPresents :: Int -> Int
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
  where factors = [ (p,q) | p <- takeWhile (<= intsqrt n) primes
                          , let (q,r) = quotRem n p
                          , r == 0 ]
        p = fst $ head factors
        q = snd $ head factors

intsqrt :: Int -> Int
intsqrt = floor . sqrt . fromIntegral

primes :: [Int]
primes = 2 : 3 : [ p | p <- [5,7..]
                     , and [p `rem` d /= 0 | d <- takeWhile (<= intsqrt p) primes]
                 ]

withMinPresents2 :: Int -> Int
withMinPresents2 n = head $ filter ((>=n `divCeil` 11)
                          . divisorSumLimit 50) [1..]

divisorSumLimit :: Int -> Int -> Int
divisorSumLimit limit n = sum . snd . Set.split ((n-1)`div`limit) . divisors $ n

divisors :: Int -> Set Int
divisors = Set.fromList . divisorList
