{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module:      Y2016.D12
Description: Advent of Code Day 12 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 12 set of problems for <adventofcode.com>.
-}
module Y2016.D12
  ( assembunnyRegister
  , assembunnyRegisterInit
  )
where

import Data.Attoparsec.Text hiding (count)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Applicative ((<|>))
import Data.Hashable (Hashable)

newtype Register = Reg Char deriving (Eq, Hashable, Show, Ord)
data Source = Static Int | R Register deriving (Eq, Ord, Show)
data Instruction
  = Copy Source Register
  | Inc Register
  | Dec Register
  | JNZ Source Int
  deriving Show

type Instructions = Vector Instruction
type Registers = HashMap Register Int

assembunnyRegister :: Char -> Text -> Int
assembunnyRegister = assembunnyRegisterInit (const 0)

assembunnyRegisterInit :: (Char -> Int) -> Char -> Text -> Int
assembunnyRegisterInit init' register (parseOnly instructionsP -> Right instructions)
  = execute instructions 0 registers HM.! Reg register
  where
    registers :: Registers
    registers = HM.fromList $ map (\(Reg x) -> (Reg x, init' x))
      $ S.toList $ foldMap getRegister instructions
    getRegister (Copy (R reg) reg') = S.fromList [reg, reg']
    getRegister (Copy (Static _) reg) = S.singleton reg
    getRegister (Inc reg) = S.singleton reg
    getRegister (Dec reg) = S.singleton reg
    getRegister (JNZ (R reg) _) = S.singleton reg
    getRegister (JNZ (Static _) _) = S.empty
assembunnyRegisterInit _ _ (parseOnly instructionsP -> Left err) = error err

execute :: Instructions -> Int -> Registers -> Registers
execute instructions pointer = go (instructions V.!? pointer)
  where
    go Nothing registers = registers
    go (Just (Copy (Static val) register)) registers
      = execute instructions (succ pointer) $ HM.adjust (const val) register registers
    go (Just (Copy (R sourceReg) register)) registers
      = execute instructions (succ pointer) $ HM.adjust (const source) register registers
      where source = registers HM.! sourceReg
    go (Just (Inc register)) registers
      = execute instructions (succ pointer) $ HM.adjust succ register registers
    go (Just (Dec register)) registers
      = execute instructions (succ pointer) $ HM.adjust pred register registers
    go (Just (JNZ (Static n) pointer')) registers
      | n /= 0 = execute instructions (pointer + pointer') registers
      | otherwise = execute instructions (succ pointer) registers
    go (Just (JNZ (R register) pointer')) registers
      | registers HM.! register /= 0 = execute instructions (pointer + pointer') registers
      | otherwise = execute instructions (succ pointer) registers

instructionsP :: Parser Instructions
instructionsP = V.fromList <$> instructionP `sepBy` endOfLine

instructionP :: Parser Instruction
instructionP = Copy <$> ("cpy" *> skipSpace *> sourceP) <* skipSpace <*> (Reg <$> letter)
               <|> Inc <$> ("inc" *> skipSpace *> (Reg <$> letter))
               <|> Dec <$> ("dec" *> skipSpace *> (Reg <$> letter))
               <|> JNZ <$> ("jnz" *> skipSpace *> sourceP) <*> (skipSpace *> signed decimal)

sourceP :: Parser Source
sourceP = Static <$> decimal <|> R . Reg <$> letter
