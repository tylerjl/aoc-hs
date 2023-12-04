{-|
Module:      Y2023.D03
Description: Advent of Code 2023 Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2023 day 03 set of problems for <adventofcode.com>.
-}
module Y2023.D03 where

import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (Sum), getSum, getProduct, Product (Product))
import Data.Either.Utils (fromRight)
import Data.Foldable (foldMap')

data Instruction = Symbol | Gear | Digit Char deriving Show
type Cell = Maybe Instruction
type Point = (Int, Int)
type Symbols = Set Point
type Parts = Map (Set Point) Int

-- |Solve for part A - just construct the two structures and them find
-- members of the map that overlay with symbol neighbor points and sum
-- them up.
part3A :: Text -> Int
part3A = getSum
       . (\(x, y) -> M.foldMapWithKey (filterAdjacent x) y)
       . fromGrid expandSymbol . fromRight . parseOnly parseSchematic
  where
    expandSymbol point set' = set' `S.union` S.fromList (neighbors point)

-- |Solve for part B, this one is a little more complicated.
-- 
-- We still parse out a set of gear points and a map of all numbers
-- and the points they cover, but we need to walk through the set of
-- gear points and filter them based upon whether they have two
-- neighbors after filtering the number map for nearby numbers.
part3B :: Text -> Int
part3B input = getSum twoGear
  where
    -- Take our set of symbol points and transform them into nearby
    -- numbers and then sum them
    twoGear = foldMap' (Sum . gearNumbers) symbols
    -- To find qualifying symbols/gears, turn the gear’s point into a
    -- set of points, and then
    gearNumbers (S.fromList . neighbors -> gearPoints)
      -- If the nearby numbers are two, multiply them
      | M.size nearNumbers == 2 = getProduct $ foldMap' Product nearNumbers
      | otherwise = 0
      -- ...and we get that count of nearby numbers by filtering the
      -- parts map for overlapping points.
      where nearNumbers = M.filterWithKey (\k _ -> not (k `S.disjoint` gearPoints)) parts
    -- Boilerplate to construct our data structures.
    (symbols, parts) =
      fromGrid constructSymbols $ fromRight $ parseOnly parseSchematic input
    constructSymbols = S.insert

-- filterAdjacent :: Monoid m => Symbols -> Set Point -> Int -> m
filterAdjacent :: (Ord a1, Applicative f, Monoid (f a2)) => Set a1 -> Set a1 -> a2 -> f a2
filterAdjacent haystack needles value
  | not (haystack `S.disjoint` needles) = pure value
  | otherwise = mempty

fromGrid :: (Point -> Set Point -> Set Point) -> [(Point, Cell)] -> (Symbols, Parts)
fromGrid f = go (S.empty, M.empty)
  where
    go acc [] = acc
    go acc ((_, Nothing):xs) = go acc xs
    go (symbols, parts) l@(((_, y), Just (Digit _)):_) = go (symbols, parts') l'
      where
        parts' = M.insert (S.fromList (map fst points)) (read $ mapMaybe (toDigit . snd) points) parts
        (points, l') = span inRowDigit l
        inRowDigit ((_, y'), Just (Digit _)) = y' == y
        inRowDigit (_, _) = False
    go (symbols, parts) ((point, _):xs) = go (f point symbols, parts) xs

neighbors :: Point -> [Point]
neighbors (x, y) =
  [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  , (x - 1, y),                 (x + 1, y)
  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

toDigit :: Maybe Instruction -> Maybe Char
toDigit (Just (Digit d)) = Just d
toDigit _ = Nothing

parseSchematic :: Parser [(Point, Cell)]
parseSchematic = concat . zipWith points [0..] <$> parseRow `sepBy` endOfLine
  where points row = map (grid row)
        grid row (col, cell) = ((col, row), cell)

parseRow :: Parser [(Int, Cell)]
parseRow = zip [0..] <$> cellP `manyTill` lookAhead endOfLine

cellP :: Parser Cell
cellP = (Nothing <$ ".")
  <|> (Just . Digit <$> digit)
  <|> (Just Gear <$ "*")
  <|> (Just Symbol <$ anyChar)
