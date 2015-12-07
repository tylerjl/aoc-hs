#!/usr/bin/env runhaskell

module Y2015.Util
    ( regularParse
    , intParser
) where

import qualified Text.Parsec as     P
import           Text.Parsec.Char   (digit)
import           Text.Parsec.String (Parser)

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

intParser :: Parser Int
intParser = read <$> P.many1 digit
