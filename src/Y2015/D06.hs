#!/usr/bin/env runhaskell

module Y2015.D06 where

import           Control.Applicative ((<|>))
import           Data.Array.Repa     (Z(..), (:.)(..))
import qualified Data.Array.Repa as  R
import           Data.List           (foldl')
import qualified Text.Parsec as      P
import           Text.Parsec.Char    (char, endOfLine)
import           Text.Parsec.String  (Parser)

import           Y2015.Util          (regularParse, intParser)

type Point = (Int, Int)

data Range = Range Point Point deriving (Eq, Show)
data Instruction = On Range
                 | Off Range
                 | Toggle Range
                 deriving (Show)

size :: Int
size = 1000

initialGrid :: R.Array R.D R.DIM2 Bool
initialGrid = R.delay $ R.fromListUnboxed
                  (Z :. size :. size :: R.DIM2)
                  (replicate (size*size) False)

instructionsParser :: Parser [Instruction]
instructionsParser = P.many (instruction <* P.optional endOfLine)

instruction :: Parser Instruction
instruction = On     <$> directive "turn on"
          <|> Off    <$> directive "turn off"
          <|> Toggle <$> directive "toggle"

directive :: String -> Parser Range
directive s = P.skipMany1 (P.try (P.string s *> P.skipMany1 P.space)) *> range

range :: Parser Range
range = Range <$> point <* P.string " through " <*> point

point :: Parser Point
point = (,) <$> intParser <* char ',' <*> intParser

configureGrid :: R.Array R.D R.DIM2 Bool
              -> Instruction
              -> R.Array R.D R.DIM2 Bool
configureGrid a (On range)     = switch a (True ||) range
configureGrid a (Off range)    = switch a (False &&) range
configureGrid a (Toggle range) = switch a xor range

switch :: (R.Source r a)
       => R.Array r R.DIM2 a
       -> (a -> a)
       -> Range
       -> R.Array R.D R.DIM2 a
switch a f r = R.traverse a id (set f r)

-- This is pretty confusing:
--    Custom mapping function (set the lights)
-- -> Range to apply the function upon
-- -> Function to retrieve original elements from
-- -> Original array constructor
-- -> New (or unchanged) value
set :: (a -> a) -> Range -> (R.DIM2 -> a) -> R.DIM2 -> a
set f (Range (x',y') (x'',y'')) g (Z :. x :. y)
    | withinX && withinY = f orig
    | otherwise          = orig
    where withinX = x >= x' && x <= x''
          withinY = y >= y' && y <= y''
          orig    = g (Z :. x :. y)

on :: Bool -> Int
on True  = 1
on False = 0

xor :: Bool -> Bool
xor True  = False
xor False = True

main :: IO ()
main = do
        input <- readFile "Y2015/D06_input"
        case regularParse instructionsParser input of
            Right instructions -> do
                putStr "Part A - total lights lit: "
                s <- R.foldAllP (+) 0 $ R.map (on) $ foldl' (configureGrid) initialGrid instructions
                print s
            Left e         -> putStrLn "Error: Malformed input:" >> print e
