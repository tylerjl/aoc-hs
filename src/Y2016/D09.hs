{-|
Module:      Y2016.D09
Description: Advent of Code Day 09 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 09 set of problems for <adventofcode.com>.
-}
module Y2016.D09
  ( inflate
  , nestedInflate
  ) where

import Control.Applicative (Alternative(..))
import Data.Attoparsec.Combinator (count)
import Data.Attoparsec.Text hiding (count)
import Data.Text hiding (concat, count, filter, length, replicate)
import Data.Tree
import Witch
import qualified Data.Attoparsec.Text as AT
import qualified Data.List as L
import qualified Data.Text as T

type Repetitions = Int
data Fragment = Marker Repetitions Text | Token Char
type Compressed = Tree Repetitions

nestedInflate :: Text -> Int
nestedInflate = foldTree decompressedTree . compressedTree . T.strip

compressedTree :: Text -> Compressed
compressedTree = unfoldTree asTree . Marker 1

asTree :: Fragment -> (Repetitions, [Fragment])
asTree (Token _) = (1, mempty)
asTree (Marker repetition (parseOnly compression -> Right fragments)) =
  (repetition, fragments)
asTree (Marker repetition _) = (repetition, [])

decompressedTree :: Repetitions -> [Repetitions] -> Repetitions
decompressedTree repeat' [] = repeat'
decompressedTree repeat' acc = sum acc * repeat'

compression :: Parser [Fragment]
compression = manyTill (marker' <|> (Token <$> letter)) endOfInput
  where
    marker' = do
      (range', repeat') <- marker
      span' <- AT.take range'
      return $ Marker repeat' span'

inflate :: Text -> Either String Int
inflate (T.strip -> input) = T.length <$> parseOnly decompress input

decompress :: Parser Text
decompress = into . concat <$> manyTill (marker' <|> L.singleton <$> letter) endOfInput
  where
    marker' = do
      (range', repeat') <- marker
      span' <- count range' anyChar
      return $ concat $ replicate repeat' span'

marker :: Parser (Int, Int)
marker = "(" *> ((,) <$> decimal <* "x" <*> decimal) <* ")"
