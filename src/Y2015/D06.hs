#!/usr/bin/env runhaskell

module Y2015.D06 where

import           Control.Applicative ((<|>))
import           Data.List           (foldl')
import           Data.Matrix
                 ( (!)
                 , Matrix
                 , fromList
                 , setElem)
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

initialGrid :: Matrix Bool
initialGrid = fromList 1000 1000 $ repeat False

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

mapRange :: (a -> a) -> Range -> Matrix a -> Matrix a
mapRange f (Range cur@(xa, ya) end@(xz, yz)) m
    | xa <= xz   = mapRange f (Range (xa+1, ya) (xz, yz)) m'
    | otherwise  = m
    where height = yz - ya
          m'     = mapCol' f cur height m

mapCol' :: (a -> a) -> Point -> Int -> Matrix a -> Matrix a
mapCol' f (x,y) n m | y <= n    = mapCol' f (x,y') n set
                    | otherwise = m
    where matrixIndex = (x+1, y+1)
          light       = f (m ! matrixIndex)
          set         = setElem light matrixIndex m
          y'          = y + 1

getLit :: Matrix Bool -> Int
getLit = foldl (\acc x -> acc + on x) 0

on :: Bool -> Int
on True  = 1
on False = 0

configureGrid :: Instruction -> Matrix Bool -> Matrix Bool
configureGrid (On range)     = mapRange (True ||)  range
configureGrid (Off range)    = mapRange (False &&) range
configureGrid (Toggle range) = mapRange xor        range

xor :: Bool -> Bool
xor True  = False
xor False = True

main :: IO ()
main = do
        input <- readFile "Y2015/D06_input"
        case regularParse instructionsParser input of
            Right instructions -> do
                putStr "Part A - total lights lit: "
                print $ getLit $ foldl' (flip configureGrid) initialGrid instructions
            Left e         -> putStrLn "Error: Malformed input:" >> print e
