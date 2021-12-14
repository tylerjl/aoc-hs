{-|
Module:      Y2021.D12
Description: Advent of Code 2021 Day 12 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 12 set of problems for <adventofcode.com>.
-}
module Y2021.D12
  ( parse12
  , parse12'
  , part12A
  , part12B
  ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Bifunctor    (second)
import Data.Either.Utils (fromRight)
import Data.List         (nub)
import Data.Map          (Map)
import Data.MultiSet     (MultiSet)
import Data.Set          (Set)
import Data.Text         (Text)
import Witch
import Y2015.Util        ((<&&>))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS

-- |This GADT helps quite a bit when comparing vaules later on
data Room = Start | BigCave Text | SmallCave Text | End
  deriving (Eq, Ord, Show)
-- |Type alias for better readability.
type Caves = Map Room (Set Room)
-- |Type alias for better readability.
type TwiceVisit = Bool
-- |Type alias for better readability.
type Journey = (TwiceVisit, MultiSet Room)

-- |Solution to part A
part12A :: Text -> Int
part12A (parse12' -> rooms)
  = length $ explore smallOnce rooms (True, mempty) Start

-- |This is the main recursive function that drives both A and B. The
-- higher-order function argument is the main difference, bit otherwise we build
-- up a list of all valid routes by traversing the overall `Map`.
explore :: (Journey -> Room -> Bool) -> Caves -> Journey -> Room -> [Journey]
explore allowed caves (twice', journey') room
  | room == End = [visited]
  | otherwise = foldMap (concatMap (explore allowed caves visited)) rooms
  where
    journey = MS.insert room journey'
    twice
      | twice' = True
      | otherwise =
        any ((isSmall . fst) <&&> ((> 1) . snd)) $ MS.toOccurList journey
    visited = (twice, journey)
    rooms = S.filter (allowed visited) <$> M.lookup room caves

-- |The predicate we use for part A, which is essentially "don't visit small
-- rooms twice"
smallOnce :: Journey -> Room -> Bool
smallOnce (_, journey) c@(SmallCave _) = c `notElem` journey
smallOnce _ Start = False
smallOnce _ _ = True

-- |Solution to part B
part12B :: Text -> Int
part12B (parse12' -> rooms)
  = length $ explore smallTwice rooms (False, mempty) Start

-- |Given our journey through the caves so far and the room we'd like to proceed
-- through, should we continue?
--
-- Part B requires a little more logic, which is primarily to visit any single
-- small cave at most two times.
smallTwice :: Journey -> Room -> Bool
smallTwice _ (BigCave _) = True
smallTwice _ Start = False
smallTwice _ End = True
smallTwice (seenTwice, j) r
  | r `MS.notMember` j = True
  | seenTwice = False
  | otherwise = True

-- |Super small utility to find small caves.
isSmall :: Room -> Bool
isSmall (SmallCave _) = True
isSmall _ = False

-- |An intermediate parsing function; once we get the raw room pairs we turn it
-- into the structure we'll work with later.
parse12' :: Text -> Map Room (Set Room)
parse12' =
  M.fromListWith S.union .
  map (second S.singleton) . nub . concatMap backpath . parse12
  where
    backpath (a, b) = [(a, b), (b, a)]

-- |Pairs of cave "rooms", which we parse very simply and postprocess later.
parse12 :: Text -> [(Room, Room)]
parse12 = fromRight . parseOnly parser
  where
    parser    = line `sepBy1` endOfLine <* atEnd
    line      = (,) <$> location <* char '-' <*> location
    location  = start' <|> end' <|> bigCave <|> smallCave
    start'    = Start                  <$  string "start"
    bigCave   = BigCave   . into @Text <$> many1 (satisfy (`elem` ['A'..'Z']))
    smallCave = SmallCave . into @Text <$> many1 (satisfy (`elem` ['a'..'z']))
    end'      = End                    <$  string "end"
