module Y2015.D19 (distinctMols) where

import Control.Arrow ((***))
import Data.Char (isLower, isUpper)
import Data.Function (on)
import Data.List (foldl', inits, isInfixOf, sortBy, tails)
import Data.List.Utils (replace)
import Data.Map.Strict (Map, fromList, keys)
import qualified Data.Map.Strict as M
import Data.Set  (Set)
import qualified Data.Set as S
import Data.Tuple (swap)

type Mol       = [String]
type Repls     = Map String (Set String)
type Compounds = Set Mol

molSteps :: String -> Int
molSteps s = subSteps 0 repls subs mol
  where input = map reverse $ lines s
        mol   = last input
        repls = fromList . map (molPair id . words) $ init input
        subs = sortBy (compare `on` length) $ keys repls

subSteps :: Int -> Map String String -> [String] -> String -> Int
subSteps _ _     []            _ = 5
subSteps n repls subs@(sub:ss) m
  | n > 10000         = 0
  | m == "e"          = n
  | sub `isInfixOf` m = subSteps (n+1) repls ss subbed
  | otherwise         = subSteps n     repls ss m
  where subbed      = replace sub replacement m
        replacement = repls M.! sub

-- subSteps :: String -> [(String, String)] -> Int
-- subSteps mol ((a,b):xs) |

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
toRepls = M.fromListWith S.union . map (molPair . words)

molPair :: [String] -> (String, Set String)
molPair [from,_,to] = (from, S.singleton to)

toMol :: String -> Mol
toMol []                                = []
toMol (x:y:ys) | isUpper x && isLower y = (x:[y]) : toMol ys
toMol (x:xs)   | isUpper x || x == 'e'  = [x]     : toMol xs
               | otherwise              = toMol xs
