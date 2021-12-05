{-|
Module:      Y2021.D04
Description: Advent of Code 2021 Day 04 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 04 set of problems for <adventofcode.com>.
-}
module Y2021.D04 where

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text
import Y2015.Util (intParser', regularParse')
import Data.List (transpose, partition)

-- |Fancy data type to represent _game_ state, not just a card
data Bingo a
  = Ended (Int, a)
  | Playing a
-- |I'm not sure if a derive would get this right
instance Functor Bingo where
  fmap f (Ended (i, c)) = Ended (i, f c)
  fmap f (Playing a) = Playing (f a)
-- |Represents a game board state
type Card = [[Square]]
-- |Small wrapper over how to record marked/unmarked squares
data Square = Marked | Unmarked Int deriving Show

-- |Solve part A
part4A :: Text -> Int
part4A (regularParse' bingoParser -> Right (ns, cs)) = solve4 head ns cs
part4A (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Solve part B
part4B :: Text -> Int
part4B (regularParse' bingoParser -> Right (ns, cs)) = solve4 last ns cs
part4B (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Fortunately both A and B are just asking slightly different questions, so we
-- have a higher-order function to determine how to pull the matching value from
-- our resultant list.
solve4 :: ([Bingo Card] -> Bingo Card) -> [Int] -> [Bingo Card] -> Int
solve4 f x = tally . f . iterateMap x

-- |Problem-defined method of scoring a card. The "in-progress" scoring isn't
-- actually ever used. Probably a bug.
tally :: Bingo Card -> Int
tally (Playing card)  = sumCard card
tally (Ended (n, card)) = n * sumCard card

-- |Yank out the total values for a card.
sumCard :: Card -> Int
sumCard = sum . map (\(Unmarked n') -> n') . filter (not . marked) . concat

-- |Our main recursive loop to walk through the "called" numbers. We return a
-- list solely of elements that meet the "ended" predicate.
iterateMap :: [Int] -> [Bingo Card] -> [Bingo Card]
iterateMap [] _ = []
iterateMap (n:nums) cards = needles ++ iterateMap nums haystacks
  where (needles, haystacks) = partition hasEnded $ mark n cards

-- |Accept a list of `Bingo Card`s and update each with the called number.
mark :: Int -> [Bingo Card] -> [Bingo Card]
mark num = map (markRows num)

-- |Update a `Card` - we make the determination to "end" a game here, so as soon
-- as its "won" the type swaps over to the ended value.
markRows :: Int -> Bingo Card -> Bingo Card
markRows n = gameEnd n . fmap (fmap $ map (markRow n))
  where
    markRow _ Marked = Marked
    markRow n' a@(Unmarked b) | b == n'   = Marked
                              | otherwise = a

-- |Small helper predicate to tell is when a `Bingo` type is over or not.
hasEnded :: Bingo a -> Bool
hasEnded (Playing _) = False
hasEnded (Ended _) = True

-- |Important function to determine if a card is a winning card. If so it
-- changes its type.
gameEnd :: Int -> Bingo Card -> Bingo Card
gameEnd _ b@(Ended _) = b
gameEnd n b@(Playing card)
  | check card || (check . transpose) card = Ended (n, card)
  | otherwise = b
  where check = any (all marked)

-- |Another little helper to tell us if a spot has been marked or not.
marked :: Square -> Bool
marked Marked       = True
marked (Unmarked _) = False

-- |This is a sort of hairy, but all-in-one, parser for the problem set input. 5
-- is a magic number for board size.
bingoParser :: Parser ([Int], [Bingo Card])
bingoParser = (,) <$> (lottoParser <* newline) <*> cardsParser <* eof
  where
    lottoParser = intParser' `sepBy1` char ',' <* newline
    rowParser   = (Unmarked <$> intParser') `sepBy1` many1 (char ' ')
    cardParser  = Playing <$> count 5 (optional (many space) *> rowParser <* endOfLine)
    cardsParser = cardParser `sepBy1` newline
