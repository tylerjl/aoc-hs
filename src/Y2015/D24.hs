module Y2015.D24  (idealEntanglement) where

import Data.List  (minimumBy, permutations)
import Data.Maybe (mapMaybe)
import Data.Ord   (comparing)

idealEntanglement :: Int    -- ^ How many compartments to divide amongst.
                  -> String -- ^ Input as a string containing newlined-separated
                            --   list of package weights.
                  -> Int    -- ^ Ideal entanglement value for the resulting
                            --   package distribution.
idealEntanglement compartments input =
    product $ head $ filter isBalanced $ head $
        filter (any isBalanced) $
        allSplits $ permutations weights
    where weights  = map read $ lines input
          balanced = sum weights `quot` compartments
          isBalanced = (==) balanced . sum

allSplits :: [[a]] -> [[[a]]]
allSplits = flip concatMap [1..] . go
    where go ps n = map (splitEvery n) ps

-- Credit to: https://stackoverflow.com/a/8700618
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null)
             . map (take n)
             . iterate (drop n)
