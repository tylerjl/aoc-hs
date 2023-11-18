{-|
Module:      Y2021.D20
Description: Advent of Code 2021 Day 20 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 20 set of problems for <adventofcode.com>.
-}
module Y2021.D20
  ( parse20
  , part20A
  , part20B
  , renderGrid
  , decompress
  , expand
  ) where

import Control.Applicative
import Control.Arrow
import Data.Attoparsec.Text
import Data.Either.Utils    (fromRight)
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Text            (Text)

import qualified Data.Map.Strict as M
import Data.List.Extra (groupSortOn, transpose)
import Data.List (sort)
import Witch
import qualified Data.Text as T

type Grid = Map (Int, Int) Int

-- |Solution to part A
part20A :: Text -> Int
part20A (parse20 -> (algo, grid)) =
  M.size $ M.filter (> 0) (iterate (decompress algo) grid !! 2)

-- |Solution to part B
part20B :: Text -> Int
part20B = error "TODO"

parse20 :: Text -> ([Int], Map (Int, Int) Int)
parse20 = second intoMap . parse20'

renderGrid :: Grid -> Text
renderGrid (groupSortOn (snd . fst) . M.toList -> rows) =
  T.unlines $ map renderRow rows
  where
    renderRow (sort -> row) = into @Text $ map render row
    render (_, 1) = '#'
    render _ = '.'

decompress :: [Int] -> Grid -> Grid
decompress algo (expand -> grid) =
  foldl' (translate algo grid) M.empty $ M.keys grid

translate :: [Int] -> Grid -> Grid -> (Int, Int) -> Grid
translate algo grid acc point = M.insert point (algo !! lit) acc
  where
    lit =
      binToDec $
      reverse $ map (\x -> M.findWithDefault 0 x grid) $ neighbors point

expand :: Grid -> Grid
expand grid = foldl' (\acc (p, v) -> M.insert p v acc) grid points
  where
    points = flip zip (repeat 0) $
      zip [pred minX .. succ maxX] (repeat (pred minY)) ++
      zip [pred minX .. succ maxX] (repeat (succ maxY)) ++
      zip (repeat (pred minX)) [pred minY .. succ maxY] ++
      zip (repeat (succ maxX)) [pred minY .. succ maxY]
    ((minX, maxX), (minY, maxY)) =
      ((minimum &&& maximum) . map fst &&& (minimum &&& maximum) . map snd) $
      M.keys grid

binToDec :: [Int] -> Int
binToDec [] = 0
binToDec (x : xs) = x + 2 * binToDec xs

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
                   , (x - 1, y),     (x, y),     (x + 1, y)
                   , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                   ]

intoMap :: [[Int]] -> Map (Int, Int) Int
intoMap =
  M.fromList .
  concat . zipWith (\a b -> zip (zip (repeat a) [0 ..]) b) [0 ..]

-- |Parse.
parse20' :: Text -> ([Int], [[Int]])
parse20' = fromRight . parseOnly (parser <* atEnd)
  where
    parser = (,) <$> lits <* endOfLine <*> (transpose <$> input <* endOfLine)
    lits = many1 lit <* endOfLine
    input = many1 lit `sepBy1` endOfLine
    lit = 1 <$ char '#' <|> 0 <$ char '.'
