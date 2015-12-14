module Y2015.D13 (solveSeating) where

import           Data.List   (nub, permutations)
import           Data.Map    (Map, findWithDefault, fromList, keys)
import qualified Data.Map as Map
import           Data.Tuple  (swap)

type Guest       = String
type Happiness   = Int
type Edge        = (Guest, Guest)
type Preferences = Map Edge Happiness

solveSeating :: String -> Int
solveSeating i = maximum $ map sum guestMoods
    where prefs        = toSeating i
          guests       = nub . uncurry (++) . unzip $ keys prefs
          pairs        = map (zip <*> tail . cycle) $ permutations guests
          arrangements = map rePair pairs
          guestMoods   = map (map (flip (findWithDefault 0) prefs)) arrangements

rePair :: [(a, a)] -> [(a, a)]
rePair []         = []
rePair ((x,y):xs) = [(x,y),(y,x)] ++ rePair xs

toSeating :: String -> Preferences
toSeating = fromList . map (parseSeating . words . init) . lines

parseSeating :: [String] -> (Edge, Happiness)
parseSeating [a,_,s,h,_,_,_,_,_,_,b] = ((a, b), hap s)
    where change = read h
          hap "gain" = change
          hap "lose" = negate change
