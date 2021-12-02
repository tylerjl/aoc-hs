{-|
Module:      Y2015.D07
Description: Advent of Code Day 07 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 07 set of problems for <adventofcode.com>.
-}

module Y2015.D07
    ( wire
    , circuitParser
    , parseCircuits
    , override
) where

import Y2015.Util (regularParse, intParser)

import           Control.Applicative   ((<|>))
import           Data.Bits             ((.&.), (.|.), shift, complement)
import           Data.Function.Memoize (memoize)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict  as   M
import           Data.Word             (Word16)
import           Text.Parsec.Char      (digit, letter, endOfLine)
import           Text.Parsec.String    (Parser)
import           Text.Parsec
    ( lookAhead
    , many
    , many1
    , optional
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

-- |Parsec parser for list of 'Instruction's
circuitParser :: Parser [Instruction] -- ^ Parser
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
    <|> try (Val .read <$> many1 digit)

arrow :: Parser ()
arrow = skipMany1 space *> string "->" *> skipMany1 space

voltageOn :: Map String Gate -> String -> Word16
voltageOn m = resolve
    where eval :: String -> Word16
          eval wire' = case M.lookup wire' m of
                       Just (Singleton x) -> atom' x
                       Just (And x y)     -> atom' x .&. atom' y
                       Just (Or x y)      -> atom' x .|. atom' y
                       Just (LShift x i)  -> shift (atom' x) i
                       Just (RShift x i)  -> shift (atom' x) (-i)
                       Just (Not x)       -> complement (atom' x)
                       Nothing            -> 0
          resolve = memoize eval
          atom' (Val i) = i
          atom' (Var v) = resolve v

-- |Constructs then returns resulting voltage from wiring spec
wire :: String        -- ^ Wire to find voltage on
     -> [Instruction] -- ^ List of instructions
     -> Word16        -- ^ Resulting voltage on indicated wire
wire s = flip voltageOn s . M.fromList . map toPair
    where toPair (Instruction g w) = (w, g)

-- |Helper function to parse 'Instruction's
parseCircuits :: String                            -- ^ Input string
              -> [Instruction] -- ^ Either parse error or 'Instruction's
parseCircuits = either err suc . regularParse circuitParser
  where err = error . show
        suc r = r

-- |Inject a manual instruction.
override :: Word16      -- ^ Value to inject into 'Instruction'.
         -> Instruction -- ^ Resulting 'Instruction'.
override s = Instruction (Singleton (Val s)) "b"
