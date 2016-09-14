{-# LANGUAGE TemplateHaskell #-}

module Y2015.D22 where

import           Control.Lens
import           Data.List  (minimumBy)
import           Data.Maybe (catMaybes)
import           Data.Ord   (comparing)
import           Data.Set   (Set, insert, member)
import qualified Data.Set   as S

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

data Effect = Effect Spell Integer
            deriving (Show)

instance Eq Effect where
    (Effect a _) == (Effect b _) = a == b

instance Ord Effect where
    (Effect a _) `compare` (Effect b _) = a `compare` b

data State = PlayerTurn
           | BossTurn
           deriving (Show)

data Game = Game
    { _boss    :: Boss
    , _effects :: Set Effect
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
        e s t = effects %~ insert (Effect s t)

affect :: Effect -> Game -> Game
affect (Effect Shield _)   = player.armor .~ 7
affect (Effect Poison _)   = boss.hp -~ 3
affect (Effect Recharge _) = player.mana +~ 101
affect (Effect _ _)        = id

stepEffects :: Game -> Game
stepEffects game =
    S.foldr affect (game' & effects %~ step) (game^.effects)
    where game' = game & player.armor .~ 0
          step  = S.filter (not.expired) . S.map decay

decay :: Effect -> Effect
decay (Effect e ttl) = Effect e (ttl - 1)

expired :: Effect -> Bool
expired (Effect _ ttl) = ttl <= 0

stepGame :: Game -> [Result]
stepGame game
    | won game =
        [Won $ game^.player.spent]
    | lost game =
        [Lost]
    | otherwise =
        case game^.state of
            BossTurn   -> stepGame $ strike game' & state .~ PlayerTurn
            PlayerTurn ->
                [ result | spell <- [ s | s <- [MagicMissile ..]
                                        , not $ s `inEffect` game'
                                        ]
                         , let nextTurn = cast spell game' & state .~ BossTurn
                         , result <- stepGame nextTurn
                         ]
            where game'  = stepEffects game

inEffect :: Spell -> Game -> Bool
inEffect spell = not . S.null . S.filter active . view effects
    where active (Effect s ttl) = s == spell

strike :: Game -> Game
strike g =
    player.life -~ max 1 (g^.boss.damage - g^.player.armor) $ g

won :: Game -> Bool
won g = g^.boss.hp <= 0

lost :: Game -> Bool
lost game = game^.player.life <= 0 ||
                game^.player.mana < 0

newGame :: String -> Game
newGame input =
    Game
        { _player  = Player
            { _life = 50
            , _armor = 0
            , _mana = 500
            , _spent = 0
            }
        , _boss    = boss
        , _effects = S.empty
        , _state   = PlayerTurn
        }
    where boss = pBoss input

pBoss :: String -> Boss
pBoss input = Boss { _hp = hp, _damage = dmg }
    where parse f = read $ last $ words $ f $ lines input
          hp      = parse head
          dmg     = parse last
