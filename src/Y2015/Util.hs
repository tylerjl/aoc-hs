{-|
Module:      Y2015.Util
Description: Shared functions for Advent of Code Solutions.
License:     MIT
Maintainer:  @tylerjl

Shared functions that support solutions to problems for  the
<adventofcode.com> challenges.
-}

module Y2015.Util
    ( (<&&>)
    , regularParse
    , intParser
) where

import           Control.Monad (liftM2)
import qualified Text.Parsec as     P
import           Text.Parsec.Char   (digit)
import           Text.Parsec.String (Parser)

-- |Combinator operator for predicates
(<&&>) :: (a -> Bool) -- ^ Predicate 1
       -> (a -> Bool) -- ^ Predicate 2
       -> a           -- ^ Predicate target
       -> Bool        -- ^ Logical and for predicates
(<&&>) = liftM2 (&&)

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

-- |Generic 'Int' parser
intParser :: Parser Int -- ^ Parsec parser for 'Int' types
intParser = read <$> P.many1 digit
