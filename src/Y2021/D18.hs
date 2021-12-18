{-|
Module:      Y2021.D18
Description: Advent of Code 2021 Day 18 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 18 set of problems for <adventofcode.com>.
-}
module Y2021.D18
  ( parse18
  , part18A
  , part18B
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Either.Utils           (fromRight)
import Data.List                   (foldl1')
import Data.Text                   (Text)

data SnailTree
  = SnailNumber Int
  | SnailPair SnailTree SnailTree
  deriving Eq
instance Show SnailTree where
  show (SnailNumber n) = show n
  show (SnailPair a b) = "[" <> show a <> "," <> show b <> "]"

-- |Solution to part A
part18A :: Text -> Int
part18A = magnitude . reduce . foldl1' combine . map reduce . parse18

combine :: SnailTree -> SnailTree -> SnailTree
combine (reduce -> a) (reduce -> b) = snailAdd a b

-- |Solution to part B
part18B :: Text -> Int
part18B (parse18 -> rows) =
  maximum [(magnitude . reduce) (combine x y) | x <- rows, y <- rows, x /= y]

snailAdd :: SnailTree -> SnailTree -> SnailTree
snailAdd = SnailPair

magnitude :: SnailTree -> Int
magnitude (SnailNumber n) = n
magnitude (SnailPair (magnitude -> l) (magnitude -> r)) = l * 3 + r * 2

explode :: SnailTree -> Either SnailTree SnailTree
explode = either (\(_, tree, _) -> Left tree) Right . go (0 :: Int)
  where
    go _ (SnailNumber n) = pure (SnailNumber n)
    go d (SnailPair (SnailNumber l) (SnailNumber r)) | d >= 4 = Left (l, SnailNumber 0, r)
    go d (SnailPair l r) =
      case go (succ d) l of
        Left (ln, l', rn) -> Left (ln, SnailPair l' (withLeft r rn), 0)
        _ -> case go (succ d) r of
          Left (ln, r', rn) -> Left (0, SnailPair (withRight l ln) r', rn)
          _ -> pure $ SnailPair l r
    withLeft (SnailNumber v)  n = SnailNumber (v + n)
    withLeft (SnailPair l r)  n = SnailPair (withLeft l n) r
    withRight (SnailNumber v) n = SnailNumber (v + n)
    withRight (SnailPair l r) n = SnailPair l (withRight r n)

split :: SnailTree -> Either SnailTree SnailTree
split s@(SnailNumber n)
  | n >= 10 = Left $ SnailPair (SnailNumber $ n `div` 2) (SnailNumber $ succ n `div` 2)
  | otherwise = Right s
split (SnailPair l r) =
  case split l of
    Left l' -> Left (SnailPair l' r)
    _ -> case split r of
      Left r' -> Left (SnailPair l r')
      _ -> Right $ SnailPair l r

reduce :: SnailTree -> SnailTree
reduce (explode -> Right (split -> Right tree)) = tree
reduce (explode -> Right (split -> Left tree)) = reduce tree
reduce (explode -> Left tree) = reduce tree

-- |Parse.
parse18 :: Text -> [SnailTree]
parse18 = fromRight . parseOnly (parser <* atEnd)
  where
    parser = snail `sepBy1` endOfLine
    snail = SnailPair <$> (char '[' *> sv <* char ',') <*> (sv <* char ']')
    sv = (SnailNumber <$> decimal) <|> snail
