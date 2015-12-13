module Y2015.D07
    ( wire
    , circuitParser
) where

import Y2015.Util (regularParse, intParser)

import           Control.Applicative   ((<|>))
import           Data.Bits             ((.&.), (.|.), shift, complement)
import           Data.Function.Memoize (memoize)
import           Data.List             (foldl')
import           Data.Map.Strict       (Map, empty, fromListWith)
import qualified Data.Map.Strict  as   M
import           Data.Word             (Word16)
import           Text.Parsec.Char      (digit, letter, endOfLine)
import           Text.Parsec.String    (Parser)
import           Text.Parsec
    ( lookAhead
    , many
    , many1
    , optional
    , sepBy
    , skipMany
    , skipMany1
    , space
    , string
    , try)

type Wire = String

data Atom = Val Word16 | Var String
          deriving (Show)

data Gate = Singleton Atom
          | And       Atom Atom
          | Or        Atom Atom
          | LShift    Atom Int
          | RShift    Atom Int
          | Not       Atom
          deriving (Show)

data Instruction = Instruction Gate Wire
                 deriving (Show)

circuitParser :: Parser [Instruction]
circuitParser = many (pInstruction <* optional endOfLine)

pInstruction :: Parser Instruction
pInstruction = Instruction <$> pGate <*> pWire

pGate :: Parser Gate
pGate =  try (Singleton <$> atom <* lookAhead arrow)
     <|> try (And       <$> atom <* gate "AND"    <*> atom)
     <|> try (Or        <$> atom <* gate "OR"     <*> atom)
     <|> try (LShift    <$> atom <* gate "LSHIFT" <*> bits)
     <|> try (RShift    <$> atom <* gate "RSHIFT" <*> bits)
     <|> try (Not       <$          gate "NOT"    <*> atom)

pWire :: Parser Wire
pWire = arrow *> many1 letter

gate :: String -> Parser ()
gate s = skipMany space *> string s *> skipMany1 space

bits :: Parser Int
bits = intParser

atom :: Parser Atom
atom =  try (Var <$> many1 letter)
    <|> try (Val <$> read <$> many1 digit)

arrow :: Parser ()
arrow = skipMany1 space *> string "->" *> skipMany1 space

voltageOn :: Map String Gate -> String -> Word16
voltageOn m = resolve
    where eval :: String -> Word16
          eval wire = case M.lookup wire m of
                      Just (Singleton x) -> atom x
                      Just (And x y)     -> atom x .&. atom y
                      Just (Or x y)      -> atom x .|. atom y
                      Just (LShift x i)  -> shift (atom x) i
                      Just (RShift x i)  -> shift (atom x) (-i)
                      Just (Not x)       -> complement (atom x)
                      Nothing            -> 0
          resolve = memoize eval
          atom (Val i) = i
          atom (Var v) = resolve v

wire :: String -> [Instruction] -> Word16
wire s = flip voltageOn s . M.fromList . map toPair
    where toPair (Instruction g w) = (w, g)

main :: IO ()
main = do
        input <- readFile "src/Y2015/D07_input"
        case regularParse circuitParser input of
            Right instructions -> do
                putStr "Part A - signal on a: "
                let signal_a = wire "a" instructions
                print signal_a
                putStr "Part B - signal on a is now: "
                let override = Instruction (Singleton (Val signal_a)) "b"
                print $ wire "a" (instructions ++ [override])
            Left e         -> putStrLn "Error: Malformed input:" >> print e
