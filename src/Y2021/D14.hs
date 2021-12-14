{-|
Module:      Y2021.D14
Description: Advent of Code 2021 Day 14 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 14 set of problems for <adventofcode.com>.
-}
module Y2021.D14
  ( parse14
  , part14A
  , part14B
  ) where

import Data.Attoparsec.Text
import Data.Either.Utils (fromRight)
import Data.Foldable     (foldl')
import Data.Map.Strict   (Map)
import Data.Monoid
import Data.MultiSet     (MultiSet)
import Data.Text         (Text)
import Data.Tuple.Extra  ((&&&))

import qualified Data.Map.Strict as M
import qualified Data.MultiSet   as S

type PolySubs = Map String Char
type Polymer = Map String (Sum Int)

-- |Solution to part A
part14A :: Text -> Int
part14A = uncurry (solve14 10) . parse14

-- |Solution to part B
part14B :: Text -> Int
part14B = uncurry (solve14 40) . parse14

-- |Day 14 solution applicable to either parts A or B. Most of the dirty work
-- happens in `react`, but this does the output calculation logic.
solve14 :: Int -> Polymer -> PolySubs -> S.Occur
solve14 iterations polymer subs =
  abs $
  uncurry subtract $
  (minimum &&& maximum) $
  map (adjust . snd) $
  S.toOccurList $ aggregate $ flip (!!) iterations $ iterate (react subs) polymer
  where
    adjust n
      | even n = n `div` 2
      | otherwise = (n + 1) `div` 2

-- |Turn our map of tracked pairs into a `MultiSet` of characters. This number
-- won't offer the solution (occurrences for each character) but just needs
-- minor adjustments later to be accurate.
aggregate :: Map [Char] (Sum S.Occur) -> MultiSet Char
aggregate (M.toList -> chains) = foldl' summate S.empty chains
  where
    summate acc (chain, getSum -> n) = foldl' addTo acc $ zip (repeat n) chain
      where
        addTo multiset (instances, sym) =
          S.insertMany sym instances multiset

-- |Function that's suitable to be fed into `iterate`. It's probably fairly
-- memory-hungry since it builds up some big structures recursively, but it does
-- the job.
react :: PolySubs -> Polymer -> Polymer
react subs polymer = foldl' expand M.empty (M.toList polymer)
  where
    expand newPoly (chain@[a, b], n) =
      case M.lookup chain subs of
        Nothing -> addSum newPoly chain n
        Just sub -> foldl' (uncurry . addSum) newPoly chains
          where chains = zip [[a, sub], [sub, b]] (repeat n)
    expand _ _ = error "unreachable"

-- |Rearrange a few arguments to make inserting a running sum easier.
addSum :: Polymer -> String -> Sum Int -> Polymer
addSum set k v = M.insertWith mappend k v set

-- |Pair up adjacent list elements into groups of two.
twos :: [a] -> [[a]]
twos (a:b:xs) = [a, b] : twos (b : xs)
twos _ = []

-- |Parse out a sequence of chars followed by the reaction translations.
parse14 :: Text -> (Polymer, PolySubs)
parse14 = fromRight . parseOnly (parser <* endOfLine <* endOfInput)
  where
    parser =
      (,) <$> (template <* endOfLine) <*>
      (M.fromList <$> sub `sepBy1` endOfLine)
    template =
      M.fromList . flip zip (repeat (Sum 1)) . twos <$> many1 cap <* endOfLine
    sub = (,) <$> caps <* string " -> " <*> cap
    caps = many1 cap
    cap = satisfy (`elem` ['A' .. 'Z'])
