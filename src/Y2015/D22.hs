{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Y2015.D22 where

import Control.Lens

data Boss = Boss
    { _damage :: Int
    , _hp     :: Int
    } deriving (Eq, Show)

data Player = Player
    { _armor :: Int
    , _life  :: Int
    , _mana  :: Int
    } deriving (Eq, Show)

data Spell = Spell
    { _cost   :: Int
    , _effect :: Effect
    , _ttl    :: Int
    } deriving (Show)

type Effect = Game -> Game

instance Show Effect where
    show e = "<effect>"

data Game = Game
    { _player  :: Player
    , _boss    :: Boss
    , _effects :: [Spell]
    } deriving (Show)

makeLenses ''Boss
makeLenses ''Game
makeLenses ''Player
makeLenses ''Spell

spMagicMissle :: Spell
spMagicMissle =
    Spell
        { _cost   = 53
        , _effect = boss.hp -~ 4
        , _ttl    = 0
        }

spDrain :: Spell
spDrain =
    Spell
        { _cost   = 73
        , _effect = (boss.hp -~ 2) . (player.life +~ 2)
        , _ttl    = 0
        }

spShield :: Spell
spShield =
    Spell
        { _cost   = 113
        , _effect = player.armor .~ 7
        , _ttl    = 6
        }

spPoison :: Spell
spPoison =
    Spell
        { _cost   = 173
        , _effect = boss.hp -~ 3
        , _ttl    = 6
        }

spRecharge :: Spell
spRecharge =
    Spell
        { _cost   = 229
        , _effect = player.mana +~ 101
        , _ttl    = 5
        }

spells :: [Spell]
spells = [spMagicMissle, spDrain, spShield, spPoison, spRecharge]

castSpell :: Spell -> Effect
castSpell spell = spell ^. effect

newGame :: String -> Game
newGame input =
    Game
        { _player = Player { _life = 50, _armor = 0, _mana = 500 }
        , _boss   = boss
        , _effects = []
        }
    where boss = pBoss input

pBoss :: String -> Boss
pBoss input = Boss { _hp = hp, _damage = dmg }
    where parse f = read $ last $ words $ f $ lines input
          hp      = parse head
          dmg     = parse last
