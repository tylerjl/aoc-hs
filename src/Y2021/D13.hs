{-|
Module:      Y2021.D13
Description: Advent of Code 2021 Day 13 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 13 set of problems for <adventofcode.com>.
-}
module Y2021.D13
  ( parse13
  , part13A
  , part13B
  ) where

import Advent.OCR
import Control.Applicative
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Bifunctor              (second, first)
import Data.Either.Utils           (fromRight)
import Data.Foldable
import Data.List.Extra             (transpose)
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Tuple.Extra            ((***))

import qualified Data.Set as S

-- |Type alias for better readability.
type Point = (Int, Int)
-- |Pretty simple; which axis to fold across.
data Axis = X | Y deriving (Eq, Show)
-- |A fold instruction from the problem set input.
data FoldGuide = FoldAt Axis Int deriving Show

-- |Solution to part A
part13A :: Text -> Int
part13A = S.size . uncurry (foldl' origami) . second (take 1) . parse13

-- |Solution to part B
part13B :: Text -> Maybe String
part13B = parseLetters . uncurry (foldl' origami) . parse13

-- |I wrote this one up to render output, but using an upstream parser gets
-- actual letters, so this might come later but is unused for now.
_displayPoints :: Set Point -> String
_displayPoints points = unlines $ transpose $ map render [0..rows]
  where
    grid = S.toList points
    (rows, cols) = (maximum *** maximum) $ unzip grid
    render r = map (curry display r) [0..cols]
    display point | S.member point points = '#'
                  | otherwise = ' '

-- |Central function that applies a "fold" to a set of points.
origami :: Set Point -> FoldGuide -> Set Point
origami points (FoldAt axis crease)
  = S.foldl' pointFold S.empty points
  where
    pointFold paper point@(x, y)
      | axis == X && x < crease = S.insert point paper
      | axis == X = S.insert (crease - (x - crease), y) paper
      | axis == Y && y < crease = S.insert point paper
      | otherwise = S.insert (x, crease - (y - crease)) paper

-- |The parsing entrypoint turns puzzle input into the final form of a set of
-- points.
parse13 :: Text -> (Set Point, [FoldGuide])
parse13 = first (foldl' (flip S.insert) S.empty) . parse13'

-- |Day 13 input is a list of points followed by fold instructions. This
-- intermediate function gets the raw values before putting them into a `Set`.
parse13' :: Text -> ([Point], [FoldGuide])
parse13' = fromRight . parseOnly (parser <* endOfInput)
  where
    parser = (,) <$> points <* endOfLine <*> foldInstr `sepBy1` endOfLine <* endOfLine
    points = point `sepBy1` endOfLine <* endOfLine
    point = (,) <$> decimal <* char ',' <*> decimal
    foldInstr = FoldAt <$> (string "fold along " *> axis) <*> (char '=' *> decimal)
    axis = (X <$ char 'x') <|> (Y <$ char 'y')
