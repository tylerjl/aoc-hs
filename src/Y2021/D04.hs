{-|
Module:      Y2021.D04
Description: Advent of Code 2021 Day 04 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 04 set of problems for <adventofcode.com>.
-}
module Y2021.D04 where

import Data.List (transpose)
import Data.Maybe (isNothing, catMaybes)
import Data.Monoid (Sum (Sum), getSum)
import Data.Set (Set)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text
import Y2015.Util (intParser', regularParse')
import qualified Data.Set as S

-- |Fancy data type to represent _game_ state, not just a card
newtype Bingo a = Bingo a deriving Show
-- |I'm not sure if a derive would get this right
instance Functor Bingo where
  fmap f (Bingo a) = Bingo (f a)
-- |Represents a game board state
data Card = CardArr [[Square]]
          | CardSet RowSet ColSet
-- |Alternative game board representation
type ColSet = Set (Set (Sum Int))
type RowSet = Set (Set (Sum Int))
-- |Small wrapper over how to record marked/unmarked squares
type Square = Maybe (Sum Int)

-- |Solve part A
part4A :: Text -> Int
part4A (regularParse' bingoParser -> Right (ns, cs)) = solve4 head ns cs
part4A (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Solve part A - with sets!
part4ASet :: Text -> Int
part4ASet (regularParse' bingoParser -> Right (ns, map (fmap intoSet) -> cs)) = solve4 head ns cs
part4ASet (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Solve part B
part4B :: Text -> Int
part4B (regularParse' bingoParser -> Right (ns, cs)) = solve4 last ns cs
part4B (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Solve part B - with sets!
part4BSet :: Text -> Int
part4BSet (regularParse' bingoParser -> Right (ns, map (fmap intoSet) -> cs)) = solve4 last ns cs
part4BSet (regularParse' bingoParser -> Left (show -> err)) = error err

-- |Transform a bingo game from multidimensional-array based to a set-based
-- game.
intoSet :: Card -> Card
intoSet (CardArr (map catMaybes -> rows)) = CardSet rows' cols'
  where rows' = S.fromList $ map S.fromList rows
        cols' = S.fromList $ map S.fromList (transpose rows)
intoSet c@(CardSet _ _) = c

-- |Fortunately both A and B are just asking slightly different questions, so we
-- have a higher-order function to determine how to pull the matching value from
-- our resultant list.
solve4 :: ([(Int, Bingo Card)] -> (Int, Bingo Card)) -> [Int] -> [Bingo Card] -> Int
solve4 f x = tally . f . iterateMap x

-- |Problem-defined method of scoring a card. The "in-progress" scoring isn't
-- actually ever used. Probably a bug.
tally :: (Int, Bingo Card) -> Int
tally (n, Bingo card)  = n * sumCard card

-- |Yank out the total values for a card.
sumCard :: Card -> Int
sumCard (CardArr board) = (getSum . mconcat . catMaybes . concat) board
sumCard (CardSet rows cols)
  | any S.null rows = sumSet rows
  | otherwise = sumSet cols
  where sumSet = getSum . S.fold mappend (Sum 0) . S.map (S.fold mappend (Sum 0))

-- |Our main recursive loop to walk through the "called" numbers. We return a
-- list solely of elements that meet the "ended" predicate.
iterateMap :: [Int] -> [Bingo Card] -> [(Int, Bingo Card)]
iterateMap [] _ = []
iterateMap (n:nums) cards = zip (repeat n) needles ++ iterateMap nums haystacks
  where (needles, haystacks) = mark ([], []) n cards

-- |Accept a list of `Bingo Card`s and update each with the called number.
mark :: ([Bingo Card], [Bingo Card]) -> Int -> [Bingo Card] -> ([Bingo Card], [Bingo Card])
mark acc _ [] = acc
mark (l, r) num (card:cards)
  | gameEnd card' = mark (card' : l, r) num cards
  | otherwise = mark (l, card' : r) num cards
  where card' = markRows num card

-- |Update a `Card` - we make the determination to "end" a game here, so as soon
-- as its "won" the type swaps over to the ended value.
markRows :: Int -> Bingo Card -> Bingo Card
markRows n (Bingo (CardArr board)) = Bingo $ CardArr $ map (map (markRow n)) board
  where
    markRow _ Nothing = Nothing
    markRow n' a@(Just (Sum b)) | b == n'   = Nothing
                                | otherwise = a
markRows n (Bingo (CardSet rows cols)) = Bingo $ CardSet (markSet rows) (markSet cols)
  where markSet = S.map (S.delete (Sum n))

-- |Important function to determine if a card is a winning card.
gameEnd :: Bingo Card -> Bool
gameEnd (Bingo (CardArr card))
  = check card || (check . transpose) card
  where check = any (all isNothing)
gameEnd (Bingo (CardSet rows cols))
  = any S.null rows || any S.null cols

-- |This is a sort of hairy, but all-in-one, parser for the problem set input. 5
-- is a magic number for board size.
bingoParser :: Parser ([Int], [Bingo Card])
bingoParser = (,) <$> (lottoParser <* newline) <*> cardsParser <* eof
  where
    lottoParser = intParser' `sepBy1` char ',' <* newline
    rowParser   = (Just . Sum <$> intParser') `sepBy1` many1 (char ' ')
    cardParser  = Bingo . CardArr <$> count 5 (optional (many space) *> rowParser <* endOfLine)
    cardsParser = cardParser `sepBy1` newline
