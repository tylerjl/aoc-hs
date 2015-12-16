module Y2015.D16 (findAunt, findRealAunt) where

import Data.List (sortBy)
import Data.Map.Strict (Map, differenceWith, differenceWithKey, fromList, size)

type Aunt = Map String Int

findRealAunt :: String -> Int
findRealAunt = findGifter (size . differenceWithKey match gifter)
    where match "cats"        target candidate = candidate `gtNothing` target
          match "trees"       target candidate = candidate `gtNothing` target
          match "pomeranians" target candidate = target `gtNothing` candidate
          match "goldfish"    target candidate = target `gtNothing` candidate
          match _             target candidate = candidate `sameNothing` target
          gtNothing a b | a > b     = Nothing
                        | otherwise = Just a

findAunt :: String -> Int
findAunt = findGifter (size . differenceWith sameNothing gifter)

sameNothing :: (Eq a) => a -> a -> Maybe a
sameNothing a b | a == b    = Nothing
                | otherwise = Just a

findGifter :: (Aunt -> Int) -> String -> Int
findGifter f aunts = snd . last . sortBy (flip compare)
                   $ zip (map f (toAunts aunts)) [1..]

gifter :: Aunt
gifter = toAunt $ words ("Sue 0: children: 3 cats: 7 samoyeds: 2 "
                         ++ "pomeranians: 3 akitas: 0 vizslas: 0 "
                         ++ "goldfish: 5 trees: 3 cars: 2 perfumes: 1")

toAunts :: String -> [Aunt]
toAunts = map (toAunt . words) . lines . filter (/= ',')

toAunt :: [String] -> Aunt
toAunt (s:n:traits) = fromList $ toPair traits
    where toPair (x:y:ys) = (init x, read y) : toPair ys
          toPair _        = []
