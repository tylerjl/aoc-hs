{-|
Module:      Y2021.D09
Description: Advent of Code 2021 Day 09 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 09 set of problems for <adventofcode.com>.
-}
module Y2021.D09
  ( part9A
  , part9B
  , parse9
  )
  where

import Data.Attoparsec.Text hiding (take)
import Data.Matrix
import Data.Text (Text)
import Data.Either.Utils (fromRight)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Monoid
import Data.Foldable (foldl')

-- |Solve part A
part9A :: Text -> Int
part9A (parse9 -> m) =
  getSum . maybe' . foldMap (fmap (Sum . succ)) $
  mapPos (solve9 m) m

solve9 :: Matrix Int -> (Int, Int) -> Int -> Maybe Int
solve9 matrix' (a, b) n
  | isBasin matrix' (a, b) n = Just n
  | otherwise = Nothing

maybe' :: Maybe a -> a
maybe' = fromMaybe (error "bad matrix")

isBasin :: Ord a => Matrix a -> (Int, Int) -> a -> Bool
isBasin matrix' (a, b) n = all (n <) (neighbors matrix' a b)

neighbors :: Matrix b -> Int -> Int -> [b]
neighbors matrix' x y = mapMaybe neighbor (adjacent x y)
  where
    neighbor (x', y') = safeGet x' y' matrix'

adjacent :: (Num a1, Num a2) => a1 -> a2 -> [(a1, a2)]
adjacent x y = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- |Solve part B
part9B :: Text -> Int
part9B (parse9 -> m) =
  foldl' (*) 1 $ take 3 $ reverse $ catMaybes $ toList $ mapPos (solve9B m) m

solve9B :: Matrix Int -> (Int, Int) -> Int -> Maybe Int
solve9B matrix' p@(x, y) n
  | isBasin matrix' (x, y) n =
    Just . length $ basin matrix' [p] $ basinNeighbors matrix' x y
  | otherwise = Nothing
  where
    basin m' trail (n'@(x', y'):ns)
      | n' `elem` trail = basin m' trail ns
      | otherwise       = basin m' (n':trail) $ basinNeighbors m' x' y' ++ ns
    basin _ trail [] = trail

basinNeighbors :: (Eq a, Num a) => Matrix a -> Int -> Int -> [(Int, Int)]
basinNeighbors matrix' x y = filter sameBasin $ adjacent x y
  where
    sameBasin (\(x', y') -> safeGet x' y' matrix' -> Just peer) = peer /= 9
    sameBasin _ = False

-- |Parse puzzle input into a list of `Int`s with faster attoparsec.
parse9 :: Text -> Matrix Int
parse9 = fromLists . fromRight . parseOnly parser
  where
    parser = line `sepBy1` endOfLine <* atEnd
    line = many1 (read . (: []) <$> digit)
