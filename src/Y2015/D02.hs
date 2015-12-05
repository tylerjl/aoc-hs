#!/usr/bin/env runhaskell

module Y2015.D02
    ( Present(..)
    , parsePresents
    , surfaceArea
    , ribbonLength
) where

import           Data.List          (foldl', sort)
import qualified Text.Parsec as     P
import           Text.Parsec.Char   (char, digit, endOfLine)
import           Text.Parsec.String (Parser)

data Present = Present Int Int Int deriving (Show, Eq)

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

presentsParser :: Parser [Present]
presentsParser = P.many1 (presentParser <* P.optional endOfLine)

presentParser :: Parser Present
presentParser = Present <$> (dimension <* char 'x')
                        <*> (dimension <* char 'x')
                        <*>  dimension

dimension :: Parser Int
dimension = read <$> P.many1 digit

parsePresents :: String -> Maybe [Present]
parsePresents s = case regularParse presentsParser s of
                       Right ps -> Just ps
                       Left _   -> Nothing

surfaceArea :: [Present] -> Int
surfaceArea = foldl' (+) 0 . map wrapping

wrapping :: Present -> Int
wrapping p@(Present l w h) = sqft p + product (smallest p)

smallest :: Present -> [Int]
smallest (Present l w h) = take 2 $ sort [l, w, h]

sqft :: Present -> Int
sqft (Present l w h) = 2*l*w + 2*w*h + 2*h*l

area :: Present -> Int
area (Present l w h) = l*w*h

ribbonLength :: [Present] -> Int
ribbonLength = sum . map presentRibbon

presentRibbon :: Present -> Int
presentRibbon p = sum (map (*2) $ smallest p) + area p

main :: IO ()
main = do
       input <- readFile "src/Y2015/D02_input"
       case regularParse presentsParser input of
            Right presents -> do
               putStr "Part A: total square feet needed: "
               print (surfaceArea presents)
               putStr "Part B: total feet of ribbon needed: "
               print (ribbonLength presents)
            Left _         -> putStrLn "Error: Malformed input."
