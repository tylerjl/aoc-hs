module Y2015.D16 where

import Data.List (sortBy)
import Data.Map.Strict (Map, differenceWith, fromList, size)

type Aunt = Map String Int

findAunt :: String -> Int
findAunt = (flip findGifter gifter) . toAunts

findGifter :: [Aunt] -> Aunt -> Int
findGifter aunts needle = snd $ last $ sortBy (flip compare) $ zip (map (size . differenceWith match needle) aunts) [1..]
    where match target candidate | target == candidate = Nothing
                                 | otherwise           = Just candidate

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
