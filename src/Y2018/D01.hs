{-|
Module:      Y2016.D01
Description: Advent of Code Day 01 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 01 set of problems for <adventofcode.com>.
-}
module Y2018.D01
  ( frequency
  ) where

import Y2015.Util (regularParse, intParser)

import Control.Applicative ((<|>))
import Data.List (foldl')
import Text.Parsec.String  (Parser)
import Text.Parsec.Char    (endOfLine)
import Text.Parsec
    ( ParseError
    , many
    , optional
    , string
    , try)

data Change = Increase Int
            | Decrease Int

parseFrequency :: String
               -> Either ParseError [Change]
parseFrequency = regularParse freqParser

freqParser :: Parser [Change]
freqParser = many (parseChange <* optional endOfLine)

parseChange :: Parser Change
parseChange = try (Increase <$ string "+" <*> intParser)
          <|> try (Decrease <$ string "-" <*> intParser)

frequency :: String -> Maybe Int
frequency input = case parseFrequency input of
                    Left _ -> Nothing
                    Right f -> Just $ foldl' freqSum 0 f

freqSum :: Int -> Change -> Int
freqSum n (Increase i) = n + i
freqSum n (Decrease i) = n - i
