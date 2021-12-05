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
import Y2015.Util (intParser', regularParse', (<||>))
import Data.List (transpose)

type Card = [[Square]]
data Square = Marked | Unmarked Int deriving Show

part4A :: Text -> Int
part4A (regularParse' bingoParser -> Right (ns, cs)) = solve4A ns cs
part4A (regularParse' bingoParser -> Left (show -> err)) = error err

solve4A :: [Int] -> [Card] -> Int
solve4A x = tally . head . filter (winner . snd) . iterateMap x

part4B :: Text -> Int
part4B = error "not implemented"

tally :: (Int, Card) -> Int
tally (n, card) =
  n * (sum . map (\(Unmarked n') -> n') $ filter (not . marked) $ concat card)

iterateMap :: [Int] -> [Card] -> [(Int, Card)]
iterateMap [] _ = []
iterateMap (n:nums) cards = zip (repeat n) cards' ++ iterateMap nums cards'
  where cards' = mark n cards

mark :: Int -> [Card] -> [Card]
mark num = map (markRows num)

markRows :: Int -> Card -> Card
markRows n = map (map (markRow n))
  where
    markRow _ Marked = Marked
    markRow n' a@(Unmarked b) | b == n'   = Marked
                              | otherwise = a

winner :: Card -> Bool
winner = check <||> (check . transpose)
  where check = any (all marked)

marked :: Square -> Bool
marked Marked       = True
marked (Unmarked _) = False

bingoParser :: Parser ([Int], [Card])
bingoParser = (,) <$> (lottoParser <* newline) <*> cardsParser <* eof
  where
    lottoParser = intParser' `sepBy1` char ',' <* newline
    rowParser   = (Unmarked <$> intParser') `sepBy1` many1 (char ' ')
    cardParser  = count 5 (optional (many space) *> rowParser <* endOfLine)
    cardsParser = cardParser `sepBy1` newline
