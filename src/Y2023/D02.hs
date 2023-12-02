{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-|
Module:      Y2023.D02
Description: Advent of Code 2023 Day 02 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2023 day 02 set of problems for <adventofcode.com>.
-}
module Y2023.D02 where

import Data.Attoparsec.Text hiding (I)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as MS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Either.Utils (fromRight)
import Data.Monoid (Sum(Sum), getSum)

data Color = Red | Green | Blue deriving (Eq, Ord, Show)
type Bag = Map Color Int
type Games = IntMap Bag

-- |Solve for part A
part2A :: Text -> Int
part2A = sum . M.keys . M.filter (maxColors needle) . parseRounds
  where
    needle = [(Red, 12), (Green, 13), (Blue, 14)]

-- |Take a list of color and shown time and determine whether the bag
-- meets the criteria.
maxColors :: [(Color, Int)] -> Bag -> Bool
maxColors colors bag = all containedIn colors
  where
    containedIn (color, max') = maxColor color max' bag

-- |Solve for part B
part2B :: Text -> Int
part2B = getSum . foldMap (Sum . product) . parseRounds

maxColor :: Color -> Int -> Bag -> Bool
maxColor color max' bag =
  case bag MS.!? color of
    Nothing -> True
    Just maxShown -> max' >= maxShown

-- |Parse the 'Game' structure from an input 'Text'.
parseRounds :: Text -> Games
parseRounds = fromRight . parseOnly parseGames

parseGames :: Parser Games
parseGames = M.fromList <$> parseGame `sepBy` endOfLine

parseGame :: Parser (Int, Bag)
parseGame = (,) <$> ("Game" *> skipSpace *> decimal <* ":" <* skipSpace) <*> parseBags

parseBags :: Parser Bag
parseBags = MS.unionsWith max <$> parseBag `sepBy` "; "

parseBag :: Parser Bag
parseBag = MS.fromList <$> parseColorCount `sepBy` ", "

parseColorCount :: Parser (Color, Int)
parseColorCount = flip (,) <$> decimal <*> (skipSpace *> parseColor)

parseColor :: Parser Color
parseColor = Red <$ "red" <|> Green <$ "green" <|> Blue <$ "blue"
