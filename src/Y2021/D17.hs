{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module:      Y2021.D17
Description: Advent of Code 2021 Day 17 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 17 set of problems for <adventofcode.com>.
-}
module Y2021.D17
  ( parse17
  , part17A
  , part17B
  , solve17A
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Either.Utils    (fromRight)
import Data.Text            (Text)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data LandingZone = LZ (Int, Int) (Int, Int)
  deriving (Generic, NFData, Show)

-- |Solution to part A
part17A :: Text -> Int
part17A = solve17A . parse17

-- |Solution to part A
solve17A :: LandingZone -> Int
solve17A (LZ _ (abs -> y1, abs -> y2)) = (y * (y - 1)) `div` 2
  where
    y = max y1 y2

-- |Solution to part B
part17B :: Text -> Int
part17B (parse17 -> LZ (x1, x2) (y1, y2)) = length $
  [ a | dx <- [1 .. x2]
  , dy <- [y1 .. negate y1]
  , let a = arc (dx, dy)
  , isValid a
  ]
  where
    isValid = any (\(x, y) -> x1 <= x && x <= x2 && y1 <= y && y <= y2)
    arc = takeWhile (\(_, y) -> y >= y1) . steps (0, 0)
      where
        steps (x, y) (dx, dy) =
          (x, y) : steps (x + dx, y + dy) (dx - signum dx, dy - 1)

-- |Parse.
parse17 :: Text -> LandingZone
parse17 = fromRight . parseOnly parser
  where
    parser = LZ <$> ("target area: " *> span') <*> (", " *> span')
    span' = sortTuple <$> range'
    range' = (,) <$> (axis *> "=" *> signed decimal) <*> (".." *> signed decimal)
    axis = char 'x' <|> char 'y'

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple t@(a, b) | a <= b = t
                   | otherwise = (b, a)
