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

data Bingo a
  = Ended (Int, a)
  | Playing a
instance Functor Bingo where
  fmap f (Ended (i, c)) = Ended (i, f c)
  fmap f (Playing a) = Playing (f a)
type Card = [[Square]]
data Square = Marked | Unmarked Int deriving Show

part4A :: Text -> Int
part4A (regularParse' bingoParser -> Right (ns, cs)) = solve4A ns cs
part4A (regularParse' bingoParser -> Left (show -> err)) = error err

solve4A :: [Int] -> [Bingo Card] -> Int
solve4A x = tally . head . iterateMap x

part4B :: Text -> Int
part4B = error "not implemented"

tally :: Bingo Card -> Int
tally (Playing card)  = sumCard card
tally (Ended (n, card)) = n * sumCard card

sumCard :: [[Square]] -> Int
sumCard = sum . map (\(Unmarked n') -> n') . filter (not . marked) . concat

iterateMap :: [Int] -> [Bingo Card] -> [Bingo Card]
iterateMap [] _ = []
iterateMap (n:nums) cards = needles ++ iterateMap nums haystacks
  where (needles, haystacks) = partition hasEnded $ mark n cards

mark :: Int -> [Bingo Card] -> [Bingo Card]
mark num = map (markRows num)

markRows :: Int -> Bingo Card -> Bingo Card
markRows n = gameEnd n . fmap (fmap $ map (markRow n))
  where
    markRow _ Marked = Marked
    markRow n' a@(Unmarked b) | b == n'   = Marked
                              | otherwise = a

hasEnded :: Bingo a -> Bool
hasEnded (Playing _) = False
hasEnded (Ended _) = True

gameEnd :: Int -> Bingo Card -> Bingo Card
gameEnd _ b@(Ended _) = b
gameEnd n b@(Playing card)
  | check card || (check . transpose) card = Ended (n, card)
  | otherwise = b
  where check = any (all marked)

marked :: Square -> Bool
marked Marked       = True
marked (Unmarked _) = False

bingoParser :: Parser ([Int], [Bingo Card])
bingoParser = (,) <$> (lottoParser <* newline) <*> cardsParser <* eof
  where
    lottoParser = intParser' `sepBy1` char ',' <* newline
    rowParser   = (Unmarked <$> intParser') `sepBy1` many1 (char ' ')
    cardParser  = Playing <$> count 5 (optional (many space) *> rowParser <* endOfLine)
    cardsParser = cardParser `sepBy1` newline
