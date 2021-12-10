{-|
Module:      Y2021.D10
Description: Advent of Code 2021 Day 10 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 10 set of problems for <adventofcode.com>.
-}
module Y2021.D10
  ( part10A
  , part10B
  , parse10
  ) where

import Control.Monad     (foldM)
import Data.Either.Extra (lefts, rights)
import Data.Foldable     (foldl')
import Data.List         (sort)
import Data.Text         (Text)
import Witch

import qualified Data.Text as T

-- |Solve part A
part10A :: Text -> Int
part10A = sum . map errorTable . lefts . map (parse10 . into @String) . T.lines

-- |Character lookup table for part A
errorTable :: Char -> Int
errorTable c | c == '(' || c == ')' = 3
errorTable c | c == '[' || c == ']' = 57
errorTable c | c == '{' || c == '}' = 1197
errorTable c | c == '<' || c == '>' = 25137
errorTable _ = 0

-- |Solve part B
part10B :: Text -> Int
part10B
  -- Then convert them to scores, custom score, and get the median
  = median . sort . map (foldl' score 0 . fmap syntaxPoints)
  -- Get all valid unterminated syntaxes
  . rights . map (parse10 . into @String) . T.lines

-- |Manual parser for a given input string. I tried a `Seq` intially, just a
-- stock `List`s as a stack works just as well.
parse10 :: [Char] -> Either Char [Char]
parse10 = fmap (map pair') . foldM go []
  where
    go [] c
      | opener c = pure [c]
      | otherwise = Left c
    go acc c | opener c = pure (c:acc)
    go ('(':acc) ')' = pure acc
    go ('{':acc) '}' = pure acc
    go ('[':acc) ']' = pure acc
    go ('<':acc) '>' = pure acc
    go _ c = Left c
    opener c = c == '(' || c == '[' || c == '{' || c == '<'

-- |Utility to lookup the pairwise values for a expression opener/closer.
pair' :: Char -> Char
pair' '(' = ')'
pair' ')' = '('
pair' '{' = '}'
pair' '}' = '{'
pair' ']' = '['
pair' '[' = ']'
pair' '<' = '>'
pair' '>' = '<'
pair'  _  = error "invalid pair"

-- |Silly little median function
median :: [a] -> a
median []    = error "empty list"
median [x]   = x
median [x,_] = x
median xs    = median $ init $ tail xs

-- |Expresses the scoring algorithm for part B, suitable for use in a fold
score :: Num a => a -> a -> a
score total n = (total * 5) + n

-- |Maps the characters for part B into their given values.
syntaxPoints :: Num p => Char -> p
syntaxPoints ')' = 1
syntaxPoints ']' = 2
syntaxPoints '}' = 3
syntaxPoints '>' = 4
syntaxPoints  _  = 0
