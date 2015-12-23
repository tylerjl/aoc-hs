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

distinctMols :: String -> Int
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
