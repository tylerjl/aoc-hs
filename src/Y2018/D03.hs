{-|
Module:      Y2018.D03
Description: Advent of Code Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}
module Y2018.D03
  ( intactInches
  , overlappedInches
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
  { iD       :: Int
  , leftEdge :: Int
  , topEdge  :: Int
  , width    :: Int
  , height   :: Int
  } deriving (Eq, Show)

type Point = (Int, Int)

intactInches :: String -> Either ParseError [Int]
intactInches input =
  case parseClaims input of
    Left e -> Left e
    Right claims ->
      Right $ map iD $ singleClaims claims $ toClothMap claims

singleClaims :: [Claim] -> Map.Map Point (Set.Set Int) -> [Claim]
singleClaims claims cloth = filter noOtherClaims claims
  where noOtherClaims claim =
          Map.null
          $ Map.filter (not . (==) 1 . Set.size)
          $ Map.filter (Set.member (iD claim)) cloth

overlappedInches :: String -> Either ParseError Int
overlappedInches input =
  case parseClaims input of
    Left e -> Left e
    Right claims ->
      Right
        $ Map.size
        $ Map.filter multipleClaims
        $ toClothMap claims
  where multipleClaims = flip (>) 1 . Set.size

toClothMap :: [Claim] -> Map.Map Point (Set.Set Int)
toClothMap claims =
  foldl' (Map.unionWith (Set.union)) (Map.empty :: Map.Map Point (Set.Set Int))
  $ map ((\x -> Map.fromSet (\_ -> Set.singleton $ fst x) $ snd x))
  $ map toPoints claims

toPoints :: Claim -> (Int, Set.Set Point)
toPoints Claim { iD = i, leftEdge = l, topEdge = t, width = w, height = h } =
  (i, Set.fromList $ concatMap (\x -> map (\y -> (x, y)) [t .. (t+h-1)]) [l .. (l+w-1)])

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
