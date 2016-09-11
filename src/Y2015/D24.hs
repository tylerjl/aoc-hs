module Y2015.D24 (idealEntanglement) where

import Data.List   (minimumBy, subsequences)
import Data.Monoid (mconcat)
import Data.Ord    (comparing)

-- |Find the ideal entanglement value for a given input of packages.
idealEntanglement :: Int    -- ^ How many compartments to divide amongst.
                  -> String {-^
                                Input as a string containing newlined-separated
                                list of package weights.
                            -}
                  -> Int    {-^
                                Ideal entanglement value for the resulting
                                package distribution.
                            -}
idealEntanglement compartments input =
    product $ minimumBy (mconcat santasPredicates) seqs
    where weights  = map read $ lines input
          balanced = sum weights `quot` compartments
          isBalanced = (==) balanced . sum
          seqs = filter isBalanced $ subsequences weights
          santasPredicates = [comparing length, comparing product]
