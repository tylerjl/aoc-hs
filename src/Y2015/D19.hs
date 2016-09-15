{-|
Module:      Y2015.D19
Description: Advent of Code Day 19 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 19 set of problems for <adventofcode.com>.
-}

module Y2015.D19 (distinctMols, molSteps) where

import Data.Char (isLower, isUpper)
import Data.List (foldl', inits, tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set  (Set)
import qualified Data.Set as S

type Mol       = [String]
type Repls     = Map String (Set String)
type Compounds = Set Mol

-- |Returns the number of steps required to form a specified molecule
molSteps :: String -- ^ Target molecule composition as a raw string
         -> Int    -- ^ Number of steps required to create indicated molecule
molSteps = (+) (-1) . sum . map replCount . toMol . last . lines
  where replCount "Rn" =  0
        replCount "Ar" =  0
        replCount "Y"  = -1
        replCount _    =  1

-- |Finds the number of possible distinct molecules
distinctMols :: String -- ^ List of starting molecules as a raw string
             -> Int    -- ^ Number of distinct modules that can be formed
distinctMols s = S.size $ compounds mols repls
  where input = lines s
        mols  = toMol $ last input
        repls = toRepls $ init input

compounds :: Mol -> Repls -> Set String
compounds m r = foldl' S.union S.empty $ map combine molTrips
  where molTrips = zip3 (inits m) m (tail $ tails m)
        combine t@(_,m,_) = subRepl t $ M.findWithDefault S.empty m r

subRepl :: (Mol, String, Mol) -> Set String -> Set String
subRepl (pre,sub,post) = foldl' (flip S.insert) S.empty
                       . map (concat . construct) . S.toList
  where construct repl = pre ++ [repl] ++ post

toRepls :: [String] -> Repls
toRepls = M.fromListWith S.union . map (molPair S.singleton . words)

molPair :: (a -> b) -> [a] -> (a, b)
molPair f [from,_,to] = (from, f to)

toMol :: String -> Mol
toMol []                                = []
toMol (x:y:ys) | isUpper x && isLower y = (x:[y]) : toMol ys
toMol (x:xs)   | isUpper x || x == 'e'  = [x]     : toMol xs
               | otherwise              = toMol xs
