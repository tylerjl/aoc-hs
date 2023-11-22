{-# LANGUAGE TupleSections #-}
{-|
Module:      Y2016.D10
Description: Advent of Code Day 10 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 10 set of problems for <adventofcode.com>.
-}
module Y2016.D10 (findBot, findOutputs) where

import Data.Attoparsec.Text hiding (count)
import Control.Applicative (Alternative(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, runState, get, put)

type Bot = Int
type Output = Int
data Destination = Bot Bot | Output Output deriving Show
data Instruction
  = ValueOp Int Bot
  | BotOp Bot Destination Destination
  deriving Show

type Bots = V.Vector (Maybe Int)
type BotLogic = Map Bot (Int -> Int -> [(Int, Destination)])
type Outputs = V.Vector [Int]
type History = Map Bot (Set Int)
type Runtime = ([(Int, Destination)], (Bots, Outputs))

findBot :: [Int] -> Text -> [Int]
findBot needle (runBots -> ((_, _), history))
  = M.keys $ M.filter (== S.fromList needle) history

findOutputs :: [Int] -> Text -> Int
findOutputs needles (runBots -> ((_, outputs), _))
  = product $ concatMap (outputs V.!) needles

runBots :: Text -> ((Bots, Outputs), History)
runBots (parseInstructions -> (bots, outputs, _, [])) = ((bots, outputs), M.empty)
runBots (parseInstructions -> (bots, outputs, logic, cmds))
  = runState (go (cmds, (bots, outputs))) M.empty
  where
    go :: Runtime -> State History (Bots, Outputs)
    go ([], finalState) = pure finalState
    go ((val, dest):rest, state) = do
      (instructions, state') <- execute val dest state logic
      go (instructions ++ rest, state')

execute
  :: Int
  -> Destination
  -> (Bots, Outputs)
  -> BotLogic
  -> State History Runtime
execute val (Output out) (bots, outputs) _
  = pure ([], (bots, V.modify (\v -> MV.write v out $ existing ++ [val]) outputs))
  where existing = outputs V.! out
execute val (Bot bot) (bots, outputs) logic
  = case bots V.!? bot of
      Nothing -> error $ "Couldn't find bot " <> show bot <> " out of " <> show (V.length bots)
      Just Nothing -> do
        state <- get
        put $ M.unionWith S.union state $ M.fromList [(bot, S.singleton val)]
        pure ([], (V.modify (add $ pure val) bots, outputs))
      Just (Just existing) -> do
        state <- get
        put $ M.unionWith S.union state $ M.fromList $ map ((bot,) . S.singleton) [existing, val]
        pure ( (logic M.! bot) existing val
             , (V.modify (add Nothing) bots, outputs)
             )
  where
    add val' vec = MV.write vec bot val'

parseInstructions :: Text -> (Bots, Outputs, BotLogic, [(Int, Destination)])
parseInstructions (parseOnly instructionP -> Right instructions)
  = ( V.replicate (succ (maximum bots)) Nothing
    , V.replicate (succ (maximum outputs)) []
    , makeBotLogic instructions
    , mapMaybe cmds instructions
    )
  where
    cmds (ValueOp val bot) = Just (val, Bot bot)
    cmds _ = Nothing
    (outputs, bots) = partitionEithers $ concatMap extract instructions
    extract (ValueOp _ b) = [Right b]
    extract (BotOp b dest1 dest2) = [Right b] ++ dest dest1 ++ dest dest2
    dest (Bot b) = [Right b]
    dest (Output o) = [Left o]
parseInstructions _ = error "parsing error"

makeBotLogic :: [Instruction] -> BotLogic
makeBotLogic = M.fromList . concatMap intoPairs
  where
    intoPairs (BotOp bot d1 d2) =
      [ (bot, \x y -> [(min x y, d1), (max x y, d2)]) ]
    intoPairs (ValueOp _ _) = []

instructionP :: Parser [Instruction]
instructionP
  = (botOperationP <|> botLogicP) `sepBy` endOfLine

botOperationP :: Parser Instruction
botOperationP = ValueOp <$> ("value " *> decimal)
                        <*> (" goes to bot " *> decimal)

botLogicP :: Parser Instruction
botLogicP = BotOp <$> ("bot " *> decimal)
                  <*> (" gives low to " *> destinationP)
                  <*> (" and high to " *> destinationP)

destinationP :: Parser Destination
destinationP = "output " *> (Output <$> decimal)
               <|> "bot " *> (Bot <$> decimal)
