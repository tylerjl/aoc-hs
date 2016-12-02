{-|
Module:      Y2015.D16
Description: Advent of Code Day 16 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 16 set of problems for <adventofcode.com>.
-}

module Y2015.D16 (findAunt, findRealAunt) where

import Data.List (maximumBy)
import Data.Map.Strict (Map, differenceWith, differenceWithKey, fromList, size)

type Aunt = Map String Int

-- |Finds the real Aunt given altered targeting criteria
findRealAunt :: String -- ^ Raw input of list of Aunts
             -> Int    -- ^ Id of the gifting Aunt
findRealAunt = findGifter (size . differenceWithKey match gifter)
    where match "cats"        target candidate = candidate `gtNothing` target
          match "trees"       target candidate = candidate `gtNothing` target
          match "pomeranians" target candidate = target `gtNothing` candidate
          match "goldfish"    target candidate = target `gtNothing` candidate
          match _             target candidate = candidate `sameNothing` target
          gtNothing a b | a > b     = Nothing
                        | otherwise = Just a

-- |Given a list of Aunts, find the Id of the gifting Aunt.
findAunt :: String -- ^ Raw input of list of Aunts
         -> Int    -- ^ Id of the gifting Aunt
findAunt = findGifter (size . differenceWith sameNothing gifter)

sameNothing :: (Eq a) => a -> a -> Maybe a
sameNothing a b | a == b    = Nothing
                | otherwise = Just a

findGifter :: (Aunt -> Int) -> String -> Int
findGifter f aunts = snd . maximumBy (flip compare)
                   $ zip (map f (toAunts aunts)) [1..]

gifter :: Aunt
gifter = toAunt $ words ("Sue 0: children: 3 cats: 7 samoyeds: 2 "
                         ++ "pomeranians: 3 akitas: 0 vizslas: 0 "
                         ++ "goldfish: 5 trees: 3 cars: 2 perfumes: 1")

toAunts :: String -> [Aunt]
toAunts = map (toAunt . words) . lines . filter (/= ',')

toAunt :: [String] -> Aunt
toAunt (_:_:traits) = fromList $ toPair traits
    where toPair (x:y:ys) = (init x, read y) : toPair ys
          toPair _        = []
toAunt _ = fromList []
