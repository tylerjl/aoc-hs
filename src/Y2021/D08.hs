{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module:      Y2021.D08
Description: Advent of Code 2021 Day 08 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 08 set of problems for <adventofcode.com>.
-}
module Y2021.D08
  ( part8A
  , part8B
  , parse8
  , solve8A
  )
  where

import Control.Applicative
import Control.DeepSeq             (NFData)
import Data.Attoparsec.Text hiding (take)
import Data.Either.Utils           (fromRight)
import Data.HashMap.Strict         (HashMap)
import Data.Hashable               (Hashable)
import Data.List                   (sort)
import Data.Map.Strict             (Map)
import Data.Maybe                  (mapMaybe)
import Data.Monoid
import Data.Text                   (Text)
import GHC.Generics                (Generic)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

-- |Guess I'll use a sum type for this
data Signal = SigA | SigB | SigC | SigD | SigE | SigF | SigG
  deriving (Bounded, Enum, Generic, Hashable, Eq, Ord, Show, NFData)

-- |Just a type alias for readability.
type SignalSequence = [Signal]
-- |Just a type alias for readability.
type Entry = ([SignalSequence], [SignalSequence])
-- |Just a type alias for readability.
type SignalEntries = [Entry]

-- |Solve part A
part8A :: Text -> Int
part8A = solve8A . parse8

-- |This one is for benching apart from the parsing overhead.
solve8A :: [(a, [SignalSequence])] -> Int
solve8A = length . filter part8APredicate . concatMap snd

-- |Our simple predicate for part A per the instructions.
part8APredicate :: SignalSequence -> Bool
part8APredicate (length -> size)
  = size == 2 || size == 4 || size == 3 || size == 7

-- |Solve part B
part8B :: Text -> Int
part8B = sum . map (uncurry freqDecode) . parse8

-- |This gets run for each line of problem input. Take the scrambled numbers, a
-- list of digits to decode, and: figure out the mapping to the real signals,
-- replace the signals in the digit input, and then turn them into numbers and
-- sum them.
freqDecode :: [SignalSequence] -> [SignalSequence] -> Int
freqDecode (composeCodes -> mappings) =
  signalSum . mapMaybe (flip HM.lookup sigMap . sort . translate)
  where
    translate signals = mapMaybe (`M.lookup` mappings) signals

-- |Take a scrambled input, and hand back what the mapping for each of its
-- "wrong" inputs are for the real signals.
composeCodes :: [SignalSequence] -> Map Signal Signal
composeCodes = M.compose freqGolden . invertMap . freqMap

-- |Utility function; take a map and invert the key/values
invertMap :: Ord b => Map a b -> Map b a
invertMap = M.fromList . map (\(x, y) -> (y, x)) . M.toList

{-|This is where the bulk of the brain wrinkling takes place.

First, accept scrambled input and run `occurrences` across it, which makes a way
to uniquely identify the signature for a signal type.

Then, we zip that with the signal itself, which lets us make a map pointing from
the frequency list to the chosen signal. Thus, the key for this map should be
the commonality between the puzzle "key", or unscrambled input, and the same
frequency function run across scrambled inputs.
-}
freqMap :: [SignalSequence] -> Map [Int] Signal
freqMap = M.fromList . flip zip all' . flip map all' . flip occurrences
  where
    all' = [minBound .. maxBound]

-- |Accept a list of signal sequences, a signal to check, and derive a
-- "signature" that indicates a) which coded input it appears in and b) how many
-- elements are in that coded input. This ends up creating a unique key that is
-- identical to the same result if you run the algorithm across a scrambled
-- input.
occurrences :: Signal -> [SignalSequence] -> [Int]
occurrences signal = sort . map length . filter (signal `elem`)

-- |Run the fancy `freqMap` over the "known good" set of signals to come up with
-- a "key" to use later.
freqGolden :: Map [Int] Signal
freqGolden = freqMap $ HM.keys sigMap

-- |The resultant list of integers needs to be adjusted to each's respective
-- position in the list (turn them into the right 10's, 100's, etc.)
signalSum :: [Int] -> Int
signalSum = getSum . mconcat . zipWith pow ([0..] :: [Int]) . reverse
  where
    pow n int = Sum $ (10 ^ n) * int

-- |These are the magic combinations from the puzzle description that indicate
-- how sequences of signals map to a digit.
sigMap :: HashMap SignalSequence Int
sigMap = HM.fromList $ zip
  [ [SigA, SigB, SigC, SigE, SigF, SigG]
  , [SigC, SigF]
  , [SigA, SigC, SigD, SigE, SigG]
  , [SigA, SigC, SigD, SigF, SigG]
  , [SigB, SigC, SigD, SigF]
  , [SigA, SigB, SigD, SigF, SigG]
  , [SigA, SigB, SigD, SigE, SigF, SigG]
  , [SigA, SigC, SigF]
  , [SigA, SigB, SigC, SigD, SigE, SigF, SigG]
  , [SigA, SigB, SigC, SigD, SigF, SigG]
  ] [0..]

-- |Parse puzzle input into a list of `Int`s with faster attoparsec.
parse8 :: Text -> SignalEntries
parse8 = fromRight . parseOnly parser
  where
    parser = entry `sepBy1` endOfLine <* endOfLine
    entry = (,) <$> pattern' <* string " | " <*> pattern'
    pattern' = many1 signal `sepBy1` satisfy isHorizontalSpace
    signal = SigA <$ char 'a'
             <|> SigB <$ char 'b'
             <|> SigC <$ char 'c'
             <|> SigD <$ char 'd'
             <|> SigE <$ char 'e'
             <|> SigF <$ char 'f'
             <|> SigG <$ char 'g'
