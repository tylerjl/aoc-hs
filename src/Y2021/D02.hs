{-# LANGUAGE FlexibleContexts #-}

module Y2021.D02 where

import Data.Text (Text)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Y2015.Util (regularParse', intParser')
import Control.Applicative ((<|>))

data Navigation
  = Forward Int
  | Down Int
  | Up Int

navParser :: Parser [Navigation]
navParser = P.many (parseNav <* P.optional P.endOfLine)

parseNav :: Parser Navigation
parseNav = Forward <$> pInstr "forward"
       <|> Down    <$> pInstr "down"
       <|> Up      <$> pInstr "up"

pInstr :: String -> Parser Int
pInstr s = P.string s *> P.skipMany1 P.space *> intParser'

part2A :: Text -> Int
part2A (regularParse' navParser -> Right navs) = uncurry (*) $ go (0, 0) navs
  where go (x, y) (Forward n:xs) = go (x + n, y) xs
        go (x, y) (Down n:xs)    = go (x, y + n) xs
        go (x, y) (Up n:xs)      = go (x, y - n) xs
        go coords []             = coords
part2A (regularParse' navParser -> Left err) = error (show err)

part2B :: Text -> Int
part2B (regularParse' navParser -> Right navs) = (\(x, y, _) -> x * y) $ go (0, 0, 0) navs
  where go (x, y, aim) (Forward n:xs) = go (x + n, y + (aim * n), aim) xs
        go (x, y, aim) (Down n:xs)    = go (x, y, aim + n) xs
        go (x, y, aim) (Up n:xs)      = go (x, y, aim - n) xs
        go coords []                  = coords
part2B (regularParse' navParser -> Left err) = error (show err)
