module Y2015.D24 (idealEntanglement) where

import Data.List (minimumBy, permutations)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

idealEntanglement :: Int    -- ^ How many compartments to divide amongst.
                  -> String -- ^ Input as a string containing newlined-separated
                            --   list of package weights.
                  -> Int    -- ^ Ideal entanglement value for the resulting
                            --   package distribution.
idealEntanglement compartments input =
    case minGroups balanced weights of
        [] -> -1
        l  -> minimum $ map product $ filter ((==) balanced . sum) l
    where weights  = map read $ lines input
          balanced = sum weights `quot` compartments

minGroups :: Int -> [Int] -> [[Int]]
minGroups balance weights =
    minimumBy (comparing groupCardinality) $
        mapMaybe (go 1 balance) orderings
    where
        orderings = permutations weights
        go n w l | n >= length l = Nothing
                 | otherwise     =
                     if not $ null balanced
                         then Just balanced
                         else go (n+1) w l
                     where
                         compartments = splitEvery n l
                         balanced = filter ((==) w . sum)
                             compartments

groupCardinality :: [[a]] -> Int
groupCardinality = minimum . map length

-- Credit to: https://stackoverflow.com/a/8700618
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)
