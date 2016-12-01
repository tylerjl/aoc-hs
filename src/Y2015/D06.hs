{-|
Module:      Y2015.D06
Description: Advent of Code Day 06 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 06 set of problems for <adventofcode.com>.
-}
module Y2015.D06
  ( testA
  , testB
  , Instruction(..)
  , Range(..)
  , parseInstructions
  , configureGridA
  , configureGridB
  , lightSimulation
  ) where

import Control.Applicative ((<|>))
import Data.List (foldl')

import qualified Data.Array.Repa as R
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Text.Parsec as P
import Text.Parsec.Char (char, endOfLine)
import Text.Parsec.String (Parser)
import Data.Vector.Unboxed.Base (Unbox)

import Y2015.Util (regularParse, intParser)

type Point = (Int, Int)

-- |Represents a two-dimensional range of lights.
data Range =
  Range Point
        Point
  deriving (Eq, Show)

-- |Type of light grid instruction.
data Instruction
  = On Range
  | Off Range
  | Toggle Range
  deriving (Show)

size :: Int
size = 1000

initialGrid :: R.Array R.U R.DIM2 Int
initialGrid =
  R.fromListUnboxed (Z :. size :. size :: R.DIM2) (replicate (size * size) 0)

instructionsParser :: Parser [Instruction]
instructionsParser = P.many (instruction <* P.optional endOfLine)

instruction :: Parser Instruction
instruction = On <$> directive "turn on"
          <|> Off <$> directive "turn off"
          <|> Toggle <$> directive "toggle"

directive :: String -> Parser Range
directive s = P.skipMany1 (P.try (P.string s *> P.skipMany1 P.space)) *> range

range :: Parser Range
range = Range <$> point <* P.string " through " <*> point

point :: Parser Point
point = (,) <$> intParser <* char ',' <*> intParser

-- |Folding function to aggregate computation for 'Instruction's per part
-- |A spec.
configureGridA
  :: R.Array R.U R.DIM2 Int -- ^ Light grid.
  -> Instruction -- ^ Operation 'Instruction'.
  -> R.Array R.U R.DIM2 Int -- ^ Resultant light grid.
configureGridA a (On r) = switch a (const 1) r
configureGridA a (Off r) = switch a (const 0) r
configureGridA a (Toggle r) = switch a toggle r

-- |Folding function to aggregate computation for 'Instruction's per part
-- |B spec.
configureGridB
  :: R.Array R.U R.DIM2 Int -- ^ Light grid.
  -> Instruction -- ^ Operation 'Instruction'.
  -> R.Array R.U R.DIM2 Int -- ^ Resultant light grid.
configureGridB a (On r) = switch a (+ 1) r
configureGridB a (Off r) = switch a dim r
configureGridB a (Toggle r) = switch a (+ 2) r

toggle :: Int -> Int
toggle 1 = 0
toggle _ = 1

dim :: Int -> Int
dim = max 0 . subtract 1

switch
  :: (R.Source r a, Unbox a)
  => R.Array r R.DIM2 a -> (a -> a) -> Range -> R.Array R.U R.DIM2 a
switch a f r = R.computeS $ R.traverse a id (set f r)

-- This is pretty confusing:
--    Custom mapping function (set the lights)
-- -> Range to apply the function upon
-- -> Function to retrieve original elements from
-- -> Original array constructor
-- -> New (or unchanged) value
set :: (a -> a) -> Range -> (R.DIM2 -> a) -> R.DIM2 -> a
set f (Range (x', y') (x'', y'')) g (Z :. x :. y)
  | withinX && withinY = f orig
  | otherwise = orig
  where
    withinX = x >= x' && x <= x''
    withinY = y >= y' && y <= y''
    orig = g (Z :. x :. y)

-- |Execute 'Instruction' and return number of lit lights per part A spec.
testA
  :: Instruction -- ^ Given 'Instruction'.
  -> Int -- ^ Number of lit lights.
testA = R.foldAllS (+) 0 . configureGridA initialGrid

-- |Execute 'Instruction' and return number of lit lights per part B spec.
testB
  :: Instruction -- ^ Given 'Instruction'
  -> Int -- ^ Number of lit lights.
testB = R.foldAllS (+) 0 . configureGridB initialGrid

-- |Parses a string into a list of 'Instruction's.
parseInstructions
  :: String -- ^ Input string to parse.
  -> Either P.ParseError [Instruction] -- ^ Either an error or parsed structure.
parseInstructions = regularParse instructionsParser

-- |Run a light simulation
lightSimulation
  :: (Monad m, Foldable t)
  => (R.Array R.U R.DIM2 Int -> a -> R.Array R.U R.DIM2 Int) -- ^ REPA Light grid
  -> t a -- ^ 'Instruction's
  -> m Int -- ^ Lit lights
lightSimulation f = R.sumAllP . foldl' f initialGrid
