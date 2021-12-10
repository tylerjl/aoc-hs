{-|
Module:      Y2015.D02
Description: Advent of Code Day 02 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}
module Y2015.D02
  ( Present(..)
  , parsePresents
  , surfaceArea
  , ribbonLength
  ) where

import Data.Attoparsec.Text hiding (take)
import Data.List (foldl', sort)
import Data.Text (Text)

-- |Represents a present in three dimensions
data Present =
  Present Int
          Int
          Int
  deriving (Eq, Show)

presentsParser :: Parser [Present]
presentsParser = many1 (presentParser <* endOfLine)

presentParser :: Parser Present
presentParser =
  Present <$> (decimal <* char 'x') <*> (decimal <* char 'x') <*> decimal

-- |Parse presents from an input string
parsePresents
  :: Text -- ^ Raw input of present dimensions
  -> Maybe [Present] -- ^ Possible list of 'Present's
parsePresents s =
  case parseOnly presentsParser s of
    Right ps -> Just ps
    Left _ -> Nothing

-- |Find total surface area from list of 'Present's
surfaceArea
  :: [Present] -- ^ List of 'Present's
  -> Int -- ^ Total surface area of all 'Present's
surfaceArea = foldl' (+) 0 . map wrapping

wrapping :: Present -> Int
wrapping p = sqft p + product (smallest p)

smallest :: Present -> [Int]
smallest (Present l w h) = take 2 $ sort [l, w, h]

sqft :: Present -> Int
sqft (Present l w h) = 2 * l * w + 2 * w * h + 2 * h * l

area :: Present -> Int
area (Present l w h) = l * w * h

-- |Find required length of ribbon for a list of presents.
ribbonLength
  :: [Present] -- ^ List of 'Present's
  -> Int -- ^ Total length of required ribbon
ribbonLength = sum . map presentRibbon

presentRibbon :: Present -> Int
presentRibbon p = sum (map (* 2) $ smallest p) + area p
