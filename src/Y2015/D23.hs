module Y2015.D23 (exInstructions, exInstructions2) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Registers   = Map Register Int
type Register    = Char
data Instruction = Half      Register
                 | Triple    Register
                 | Increment Register
                 | Jump      Int
                 | JIE       Register Int
                 | JIO       Register Int
                 deriving (Show)

exInstructions :: String -> Registers
exInstructions = runInsts 0 M.empty . toInstructions

exInstructions2 :: String -> Registers
exInstructions2 = runInsts 0 (M.singleton 'a' 1) . toInstructions

runInsts :: Int -> Registers -> [Instruction] -> Registers
runInsts eip rs is | eip < 0 || eip >= length is = rs
                   | otherwise = case is !! eip of
  (Half r)      -> runInsts (eip+1) (M.adjust (`div` 2) r   rs) is
  (Triple r)    -> runInsts (eip+1) (M.adjust (*3)      r   rs) is
  (Increment r) -> runInsts (eip+1) (M.insertWith (+)   r 1 rs) is
  (Jump j)      -> runInsts (eip+j) rs is
  (JIE r i)     -> runInsts (eip+jumpTest i even   r) rs is
  (JIO r i)     -> runInsts (eip+jumpTest i (== 1) r) rs is
  where jumpTest offset f reg = if f $ M.findWithDefault 0 reg rs
                                then offset
                                else 1

toInstructions :: String -> [Instruction]
toInstructions = map (toOperation . words) . lines

toOperation :: [String] -> Instruction
toOperation ["hlf",[r]]          = Half r
toOperation ["tpl",[r]]          = Triple r
toOperation ["inc",[r]]          = Increment r
toOperation ["jmp",offset]       = Jump  $ toInt offset
toOperation ["jie",r:",",offset] = JIE r $ toInt offset
toOperation ["jio",r:",",offset] = JIO r $ toInt offset

toInt :: String -> Int
toInt ('-':s) = negate $ read s
toInt ('+':s) = read s
