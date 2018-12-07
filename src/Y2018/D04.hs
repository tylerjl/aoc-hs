{-|
Module:      Y2018.D04
Description: Advent of Code Day 03 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 04 set of problems for <adventofcode.com>.
-}
module Y2018.D04 where

import Y2015.Util (regularParse, intParser)

import qualified Data.Map.Strict as Map

import Control.Applicative ((<|>))
import Data.List           (foldl', maximumBy, sort)
import Data.Ord            (comparing)
import Text.Parsec.String  (Parser)
import Text.Parsec.Char    (endOfLine)
import Text.Parsec
    ( ParseError
    , many
    , optional
    , space
    , string
    )

data Log = Log
  { timeStamp :: TimeStamp
  , entry     :: Entry
  } deriving (Eq, Ord, Show)

data TimeStamp = TimeStamp
  { year   :: Int
  , month  :: Int
  , day    :: Int
  , hour   :: Int
  , minute :: Int
  } deriving (Eq, Ord, Show)

data Entry = StartShift Guard
           | Sleep
           | Wake
           deriving (Eq, Ord, Show)

type Guard = Int

type GuardHistory = (Maybe Guard, Map.Map Guard Shift)

data Shift = Shift
  { minutesSlept :: Map.Map Int Int
  , lastChange   :: TimeStamp
  } deriving (Eq, Ord, Show)

laziestGuard :: String -> Either ParseError Int
laziestGuard input = case parseLog input of
  Left e -> Left e
  Right logs ->
    let sleepiestMinute = fst $ maximumBy (\m c -> compare (snd m) (snd c)) $ Map.toList $ minutesSlept $ snd guard
        guardID = fst guard
        guard =
          maximumBy (comparing (Map.foldl (+) 0 . minutesSlept . snd))
          $ Map.toList
          $ snd
          $ foldl' recordLog (Nothing, Map.empty)
          $ sort logs
    in Right $ guardID * sleepiestMinute

recordLog :: GuardHistory -> Log -> GuardHistory
recordLog (_current, h) (Log { timeStamp = ts, entry = (StartShift g) }) =
  (Just g, Map.insertWith shiftChange g toShift h)
  where toShift =
          Shift
            { minutesSlept = Map.empty
            , lastChange   = ts
            }
        shiftChange _newShift oldShift =
          oldShift { lastChange = ts }
recordLog (Just current, h) (Log { timeStamp = ts@(TimeStamp { minute = m }), entry = (Wake) }) =
  (Just current, Map.adjust transition current h)
  where transition oldShift@(Shift { lastChange = (TimeStamp { minute = m' }), minutesSlept = minutes }) =
          oldShift
            { lastChange = ts
            , minutesSlept = Map.unionWith (+) minutes $ Map.fromList $ zip [m' .. (m - 1)] $ repeat 1
            }
recordLog (Just current, h) (Log { timeStamp = ts, entry = (Sleep) }) =
  (Just current, Map.adjust transition current h)
  where transition oldShift = oldShift { lastChange = ts }
recordLog gh@(Nothing, _) _ = gh

-- Parsing

parseLog :: String
         -> Either ParseError [Log]
parseLog = regularParse logParser

logParser :: Parser [Log]
logParser = many (parseRawLog <* optional endOfLine)

parseRawLog :: Parser Log
parseRawLog = Log <$> (parseTimeStamp <* space) <*> parseEntry

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = TimeStamp <$ string "[" <*> intParser <* string "-"
                          <*> intParser  <*  string "-"
                          <*> intParser  <*  space
                          <*> intParser  <*  string ":"
                          <*> intParser  <*  string "]"

parseEntry :: Parser Entry
parseEntry = StartShift <$ string "Guard #" <*> intParser <* string " begins shift"
         <|> Sleep      <$ string "falls asleep"
         <|> Wake       <$ string "wakes up"
