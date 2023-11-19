{-|
Module:      Y2016.D09
Description: Advent of Code Day 09 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 09 set of problems for <adventofcode.com>.
-}
module Y2016.D09
  ( inflate
  ) where

import Control.Applicative (Alternative(..))
import Data.Attoparsec.Combinator (count)
import Data.Attoparsec.Text hiding (count)
import Data.Text hiding (concat, count, replicate)
import Witch
import qualified Data.List as L
import qualified Data.Text as T

inflate :: Text -> Either String Int
inflate input = T.length <$> parseOnly decompress input

decompress :: Parser Text
decompress = into . concat <$> manyTill (marker <|> L.singleton <$> letter) endOfInput
  where
    -- marker :: Integral a => Parser (a, a)
    marker = do
      (range', repeat') <- "(" *> ((,) <$> decimal <* "x" <*> decimal) <* ")"
      span' <- count range' anyChar
      return $ concat $ replicate repeat' span'
