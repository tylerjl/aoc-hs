{-|
Module:      Y2015.D01
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}
module Y2015.D01
  ( level
  , basement
  ) where

import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Text (Text)
import Data.Either.Utils

-- |Parse day 1's input.
parse1 :: Text -> [Int]
parse1 = fromRight . parseOnly parser
  where
    parser = many1 (read . (: []) <$> digit) <* endOfLine <* atEnd

-- |Find final level from list of elevator movements
level
  :: Text -- ^ List of input open/close parens
  -> Int  -- ^ Final elevator level
level = foldl' (+) 0 . parse1

-- |Find position that arrives at level 0
basement
  :: Text      -- ^ List of input open/close parens
  -> Maybe Int -- ^ Possible position in string that arrives at zero
basement = find 0 1 . parse1
  where
    find current idx (x:xs)
      | current + x < 0 = Just idx
      | otherwise = find (current + x) (idx + 1) xs
    find _ _ [] = Nothing
