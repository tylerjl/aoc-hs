{-|
Module:      Y2021.D19
Description: Advent of Code 2021 Day 19 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 19 set of problems for <adventofcode.com>.
-}
module Y2021.D19
  ( parse19
  , part19A
  , part19B
  ) where

import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Either.Utils           (fromRight)
import Data.Text                   (Text)
import Data.Map.Strict             (Map)
import Data.Set                    (Set)
import Data.List                   (delete)
import Control.Arrow

import qualified Data.Set        as S
import qualified Data.Map.Strict as M

type Reading = (Int, Int, Int)
data Scanner = Scanner Int [Reading] deriving Show
type Offsets = Map (Set Float) (Set (Int, Int, Int))

-- |Solution to part A
part19A :: Text -> Int
part19A (parse19 -> scanners) = length matches
  where
    offsets = map (scanNo &&& intoOffsets) scanners
    matches = map commonPoints offsets
    commonPoints offset =
      map (assocScan offset) (offset `delete` offsets)
    assocScan (nNo, nMap) (hNo, hMap) =
      ( nNo
      , hNo
      , [ (nP, hP)
        | [(nK, nP), (hK, hP)] <- sequence [M.toList nMap, M.toList hMap]
        , let m = nK `S.intersection` hK
        , S.size m >= 11
        ])

scanNo :: Scanner -> Int
scanNo (Scanner n _) = n

intoOffsets :: Scanner -> Offsets
intoOffsets (Scanner _ readings) = M.fromList $ map intoOffset readings
  where
    intoOffset r@(fromIntegral -> x1, fromIntegral -> y1, fromIntegral -> z1) =
      ( S.fromList
          [ sqrt (((x2 - x1) ** 2) + ((y2 - y1) ** 2) + ((z2 - z1) ** 2))
          | o@(fromIntegral -> x2, fromIntegral -> y2, fromIntegral -> z2) <-
              readings
          , r /= o
          ]
      , S.singleton r)

-- |Solution to part B
part19B :: Text -> Int
part19B = error "TODO"

-- |Parse.
parse19 :: Text -> [Scanner]
parse19 = fromRight . parseOnly (parser <* atEnd)
  where
    parser = scanner `sepBy1` endOfLine
    scanner = Scanner <$> header <*> ((reading `sepBy1` endOfLine) <* endOfLine)
    header = string "--- scanner " *> decimal <* string " ---" <* endOfLine
    reading = (,,) <$> p <*> p <*> p'
    p = p' <* char ','
    p' = signed decimal
