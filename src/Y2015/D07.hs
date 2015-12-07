#!/usr/bin/env runhaskell

module Y2015.D07
    ( solve
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
    ( many
    , many1
    , optional
    , sepBy
    , skipMany
    , skipMany1
    , space
    , string
    , try)

type Wire        = String
data Atom        = Val Word16 | Var String
                 deriving (Show)
data Instruction = Singleton Atom      Wire
                 | And       Atom Atom Wire
                 | Or        Atom Atom Wire
                 | LShift    Atom Int  Wire
                 | RShift    Atom Int  Wire
                 | Not       Atom      Wire
                 deriving (Show)

class Label a where
    label :: a -> String

instance Label Instruction where
    label (Singleton _ s) = s
    label (And _ _ s)     = s
    label (Or _ _ s)      = s
    label (LShift _ _ s)  = s
    label (RShift _ _ s)  = s
    label (Not _ s)       = s

circuitParser :: Parser [Instruction]
circuitParser = many (instruction <* optional endOfLine)

instruction :: Parser Instruction
instruction = try (Singleton <$> atom                              <*> wire)
          <|> try (And       <$> atom <* gate "AND"    <*> atom    <*> wire)
          <|> try (Or        <$> atom <* gate "OR"     <*> atom    <*> wire)
          <|> try (LShift    <$> atom <* gate "LSHIFT" <*> bits    <*> wire)
          <|> try (RShift    <$> atom <* gate "RSHIFT" <*> bits    <*> wire)
          <|> try (Not       <$          gate "NOT"    <*> atom    <*> wire)

wire :: Parser Wire
wire = arrow *> many1 letter

gate :: String -> Parser ()
gate s = skipMany space *> string s *> skipMany1 space

bits :: Parser Int
bits = intParser

atom :: Parser Atom
atom = try (Var <$> many1 letter)
   <|> try (Val <$> read <$> many1 digit)

arrow :: Parser ()
arrow = skipMany1 space *> string "->" *> skipMany1 space

voltageOn :: Map String Instruction -> String -> Word16
voltageOn m = resolve
    where eval :: String -> Word16
          eval wire = case M.lookup wire m of
                      Just (Singleton x _) -> atom x
                      Just (And x y _)     -> atom x .&. atom y
                      Just (Or x y _)      -> atom x .|. atom y
                      Just (LShift x i _)  -> shift (atom x) i
                      Just (RShift x i _)  -> shift (atom x) (-i)
                      Just (Not x _)       -> complement (atom x)
                      Nothing              -> 0
          resolve = memoize eval
          atom (Val i) = i
          atom (Var v) = resolve v

solve :: String -> [Instruction] -> Word16
solve s = flip voltageOn s . M.fromList . map toPair
    where toPair inst = (label inst, inst)

main :: IO ()
main = do
        input <- readFile "src/Y2015/D07_input"
        case regularParse circuitParser input of
            Right instructions -> do
                putStr "Part A - signal on a: "
                print $ solve "a" instructions
            Left e         -> putStrLn "Error: Malformed input:" >> print e
