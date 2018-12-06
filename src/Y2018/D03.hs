{-|
Module:      Y2018.D03
Description: Advent of Code Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}
module Y2018.D03
  ( overlappedInches
  ) where

import Y2015.Util (regularParse, intParser)

import Data.List          (foldl')
import Text.Parsec.String (Parser)
import Text.Parsec.Char   (endOfLine)
import Text.Parsec
    ( ParseError
    , many
    , optional
    , space
    , string
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Claim = Claim
  { _id      :: Int
  , leftEdge :: Int
  , topEdge  :: Int
  , width    :: Int
  , height   :: Int
  } deriving (Eq, Show)

type Point = (Int, Int)

overlappedInches :: String -> Either ParseError Int
overlappedInches input =
  case parseClaims input of
    Left e -> Left e
    Right claims ->
      Right
        $ Map.size
        $ Map.filter multipleClaims
        $ foldl' (Map.unionWith (+)) (Map.empty :: Map.Map Point Int)
        $ map (Map.fromSet (\_ -> 1))
        $ map toPoints claims
  where multipleClaims = flip (>) 1

toPoints :: Claim -> Set.Set Point
toPoints Claim { leftEdge = l, topEdge = t, width = w, height = h } =
  Set.fromList $ concatMap (\x -> map (\y -> (x, y)) [t .. (t+h-1)]) [l .. (l+w-1)]

-- Parsing utilities

parseClaims :: String
           -> Either ParseError [Claim]
parseClaims = regularParse claimParser

claimParser :: Parser [Claim]
claimParser = many (parseClaim <* optional endOfLine)

parseClaim :: Parser Claim
parseClaim = Claim <$ string "#" <*> intParser <* space <* string "@" <* space
                                 <*> intParser <* string ","
                                 <*> intParser <* string ":" <* space
                                 <*> intParser <* string "x"
                                 <*> intParser
