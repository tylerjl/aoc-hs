{-|
Module:      Y2015.D02
Description: Advent of Code Day 02 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 02 set of problems for <adventofcode.com>.
-}

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

import           Y2015.Util         (regularParse, intParser)

-- |Represents a present in three dimensions
data Present = Present Int Int Int
             deriving (Show, Eq)

presentsParser :: Parser [Present]
presentsParser = P.many1 (presentParser <* P.optional endOfLine)

presentParser :: Parser Present
presentParser = Present <$> (intParser <* char 'x')
                        <*> (intParser <* char 'x')
                        <*>  intParser

-- |Parse presents from an input string
parsePresents :: String          -- ^ Raw input of present dimensions
              -> Maybe [Present] -- ^ Possible list of 'Present's
parsePresents s = case regularParse presentsParser s of
                       Right ps -> Just ps
                       Left _   -> Nothing

-- |Find total surface area from list of 'Present's
surfaceArea :: [Present] -- ^ List of 'Present's
            -> Int       -- ^ Total surface area of all 'Present's
surfaceArea = foldl' (+) 0 . map wrapping

wrapping :: Present -> Int
wrapping p@(Present l w h) = sqft p + product (smallest p)

smallest :: Present -> [Int]
smallest (Present l w h) = take 2 $ sort [l, w, h]

sqft :: Present -> Int
sqft (Present l w h) = 2*l*w + 2*w*h + 2*h*l

area :: Present -> Int
area (Present l w h) = l*w*h

-- |Find required length of ribbon for a list of presents.
ribbonLength :: [Present] -- ^ List of 'Present's
             -> Int       -- ^ Total length of required ribbon
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
