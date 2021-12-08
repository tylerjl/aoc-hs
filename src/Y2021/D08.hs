{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module:      Y2021.D08
Description: Advent of Code 2021 Day 08 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 08 set of problems for <adventofcode.com>.
-}
module Y2021.D08 where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text hiding (take)
import Data.Either.Utils (fromRight)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (permutations, find)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

-- |Guess I'll use a sum type for this
data Signal = SigA | SigB | SigC | SigD | SigE | SigF | SigG
  deriving (Bounded, Enum, Generic, Hashable, Eq, Ord, Show, NFData)

-- |These are just for readability
type SignalSequence = [Signal]
type SignalSet = Set Signal
type SignalEntries = [([SignalSequence], [SignalSequence])]

-- |Another sum type for "display"
data Display = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Show)

-- |Solve part A
part8A :: Text -> Int
part8A = solve8A . parseSevSeg

-- |This one is for benching apart from the parsing overhead.
solve8A :: [(a, [SignalSequence])] -> Int
solve8A = length . filter part8APredicate . concatMap snd

-- |Our simple predicate for part A per the instructions.
part8APredicate :: SignalSequence -> Bool
part8APredicate (length -> size)
  = size == 2 || size == 4 || size == 3 || size == 7

-- |Solve part B
part8B :: Text -> Int
part8B = getSum . mconcat . map (uncurry decode) . parseSevSeg

-- |Brute-force the input to decode the output.
decode :: [SignalSequence] -> [SignalSequence] -> Sum Int
decode (untangle -> Just decoded) =
  mconcat . zipWith pow ([0..] :: [Int]) . reverse . mapMaybe decode'
  where decode' (S.fromList . map tr -> signals) =
          fromEnum <$> M.lookup signals sigMap
        tr sig = M.findWithDefault sig sig decoded
        pow n int = Sum $ (10 ^ n) * int
decode (untangle -> Nothing) = error . mappend "could not decode" . show

-- Come up with the right mapping between scrambled signals and their true outputs.
untangle :: [SignalSequence] -> Maybe (HashMap Signal Signal)
untangle (S.fromList . map S.fromList -> coded) = find mapping all'
  where
    all' = map (M.fromList . zip [ minBound .. maxBound ])
      $ permutations [ minBound .. maxBound ]
    mapping t = S.map (S.map $ replace t) coded == correctSet
    replace test entry = M.findWithDefault entry entry test

-- |Compile the mappings for the golden input.
correctSet :: Set SignalSet
correctSet = S.fromList $ map (S.fromList . dMap') [minBound .. maxBound]

-- |Mappings from the right signals to their associated displays.
sigMap :: HashMap SignalSet Display
sigMap = M.fromList $ zip (map (S.fromList . dMap') displays) displays
  where displays = [ minBound .. maxBound ]

-- |From `Display` -> the right sequence.
dMap' :: Display -> SignalSequence
dMap' D0 = [SigA, SigB, SigC, SigE, SigF, SigG]
dMap' D1 = [SigC, SigF]
dMap' D2 = [SigA, SigC, SigD, SigE, SigG]
dMap' D3 = [SigA, SigC, SigD, SigF, SigG]
dMap' D4 = [SigB, SigC, SigD, SigF]
dMap' D5 = [SigA, SigB, SigD, SigF, SigG]
dMap' D6 = [SigA, SigB, SigD, SigE, SigF, SigG]
dMap' D7 = [SigA, SigC, SigF]
dMap' D8 = [SigA, SigB, SigC, SigD, SigE, SigF, SigG]
dMap' D9 = [SigA, SigB, SigC, SigD, SigF, SigG]

-- |Parse puzzle input into a list of `Int`s with faster attoparsec.
parseSevSeg :: Text -> SignalEntries
parseSevSeg = fromRight . parseOnly parser
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
