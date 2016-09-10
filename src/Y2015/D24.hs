module Y2015.D24 (idealEntanglement) where

import Data.List (sortBy, subsequences)

idealEntanglement :: Int    -- ^ How many compartments to divide amongst.
                  -> String -- ^ Input as a string containing newlined-separated
                            --   list of package weights.
                  -> Int    -- ^ Ideal entanglement value for the resulting
                            --   package distribution.
idealEntanglement compartments input =
    product $ head seqs
    where weights  = map read $ lines input
          balanced = sum weights `quot` compartments
          isBalanced = (==) balanced . sum
          seqs = sortBy idealPackage $
              filter isBalanced $
              subsequences weights

idealPackage a b | length a == length b = product a `compare` product b
                 | otherwise = length a `compare` length b
