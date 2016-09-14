{-# LANGUAGE TemplateHaskell #-}

module Y2015.D22
    ( Result(..)
    , spellBattle
    , testPlayer
    )
where

import           Control.Lens
import           Data.List    (foldr, minimumBy)
import           Data.Maybe   (catMaybes)
import           Data.Ord     (comparing)
import           Data.Map     (Map, insert, keys, member)
import qualified Data.Map     as M

data Boss = Boss
    { _damage :: Int
    , _hp     :: Int
    } deriving (Eq, Show)

data Player = Player
    { _armor :: Int
    , _life  :: Int
    , _mana  :: Int
    , _spent :: Int
    } deriving (Eq, Show)

data State = PlayerTurn
           | BossTurn
           deriving (Show)

data Game = Game
    { _boss    :: Boss
    , _effects :: Map Spell Int
    , _hard    :: Bool
    , _player  :: Player
    , _state   :: State
    } deriving (Show)

data Spell = MagicMissile
           | Drain
           | Shield
           | Poison
           | Recharge
           deriving (Enum, Eq, Ord, Show)

data Result = Won Int
            | Lost
            deriving (Eq, Ord, Show)

makeLenses ''Boss
makeLenses ''Game
makeLenses ''Player

cast :: Spell -> Game -> Game
cast spell = action
           . (player.mana -~ cost)
           . (player.spent +~ cost)
    where
        (action, cost) = case spell of
            MagicMissile -> (boss.hp -~ 4, 53)
            Drain        -> ((boss.hp -~ 2) . (player.life +~ 2), 73)
            Shield       -> (e Shield 6, 113)
            Poison       -> (e Poison 6, 173)
            Recharge     -> (e Recharge 5, 229)
        e s t = effects %~ insert s t

affect :: Spell -> Game -> Game
affect Shield   = player.armor .~ 7
affect Poison   = boss.hp -~ 3
affect Recharge = player.mana +~ 101
affect _        = id

stepEffects :: Game -> Game
stepEffects game =
    foldr affect (game' & effects %~ step) (keys $ game^.effects)
    where game' = game & player.armor .~ 0
          step  = M.filter (0 <) . M.map pred

stepGame :: Game -> [Result]
stepGame game
    | game^.boss.hp <= 0 =
        [Won $ game^.player.spent]
    | game^.player.life <= 0 || game^.player.mana < 0 =
        [Lost]
    | otherwise =
        case game^.state of
            BossTurn   -> stepGame $ strike game' & state .~ PlayerTurn
            PlayerTurn -> stepSpells game'
            where game' = stepEffects $ case (game^.hard, game^.state) of
                              (True, PlayerTurn) -> game & player.life -~ 1
                              _ -> game

stepSpells :: Game -> [Result]
stepSpells game =
    [ result | spell <-
        [ s | s <- [MagicMissile ..]
            , not $ member s $ game^.effects
            ]
        , let nextTurn = cast spell game & state .~ BossTurn
        , result <- stepGame nextTurn
    ]

strike :: Game -> Game
strike g =
    player.life -~ max 1 (g^.boss.damage - g^.player.armor) $ g

newGame :: Bool -> String -> Game
newGame hardMode input =
    Game
        { _player  = Player
            { _life = 50
            , _armor = 0
            , _mana = 500
            , _spent = 0
            }
        , _boss    = boss
        , _effects = M.empty
        , _state   = PlayerTurn
        , _hard    = hardMode
        }
    where boss = pBoss input

pBoss :: String -> Boss
pBoss input = Boss { _hp = hp, _damage = dmg }
    where parse f = read $ last $ words $ f $ lines input
          hp      = parse head
          dmg     = parse last

spellBattle :: Bool -> String -> Result
spellBattle hardMode = minimum . stepGame . newGame hardMode

testPlayer :: Bool -> String -> Game
testPlayer d = (player.mana .~ 250) . (player.life .~ 10) . newGame d
