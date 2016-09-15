{-|
Module:      Y2015.D24
Description: Advent of Code Day 24 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 24 set of problems for <adventofcode.com>.
-}

module Y2015.D24
    ( idealEntanglement
    , idealEntanglementOptimized
    )
where

import Data.Function (on)
import Data.List     (find, groupBy, minimumBy, sortBy, subsequences)
import Data.Monoid   (mconcat)
import Data.Ord      (comparing)

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

-- |Find the ideal entanglement value for a given input of packages.
idealEntanglementOptimized :: Int    -- ^ How many compartments to divide amongst.
                  -> String    {-^
                                   Input as a string containing newlined-separated
                                   list of package weights.
                               -}
                  -> Maybe Int {-^
                                   Ideal entanglement value for the resulting
                                   package distribution if one can be found.
                               -}
idealEntanglementOptimized compartments input =
    case seqs of
        Just groups -> Just $ minimum $ map product groups
        Nothing     -> Nothing
    where weights  = map read $ lines input
          balanced = sum weights `quot` compartments
          isBalanced = (==) balanced . sum
          seqs = find (any isBalanced) $
              groupBy ((==) `on` length) $
              sortBy (compare `on` length) $
              subsequences weights
