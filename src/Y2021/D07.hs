{-|
Module:      Y2021.D07
Description: Advent of Code 2021 Day 07 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 07 set of problems for <adventofcode.com>.
-}
module Y2021.D07 where

import Data.Attoparsec.Text hiding (take)
import Data.Either.Utils (fromRight)
import Data.Function.Memoize (memoize)
import Data.Text (Text)

-- |Solve part A
part7A :: Text -> Int
part7A = solve7 id . parseCrabs

-- |Solve part B
part7B :: Text -> Int
part7B = solve7 crabCost . parseCrabs

-- |Solve part B using a memoization strategy
part7BM :: Text -> Int
part7BM = solve7 (memoize crabCost) . parseCrabs

-- |Function to accept a given `Int` and return the sum of an origin point to
-- the given `Int`. Suitable for memoization... I think?
crabCost :: Int -> Int
crabCost = sum . flip take (iterate succ 1)

-- |Higher-order function to find the lowest fuel cost.
solve7 :: (Ord a, Ord b, Enum b, Num a, Num b) => (b -> a) -> [b] -> a
solve7 f crabs = minimum $ map costTo [0..maximum crabs]
  where costTo crab = sum [ (f . abs) (crab - dest) | dest <- crabs ]

-- |Parse puzzle input into a list of `Int`s with faster attoparsec.
parseCrabs :: Text -> [Int]
parseCrabs = fromRight . parseOnly parser
  where parser = decimal `sepBy` char ',' <* endOfLine
