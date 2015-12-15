module Y2015.D14 (distanceRace, leadingRace) where

import Data.List (foldl', maximumBy)

data Reindeer = Reindeer
              { name      :: String
              , velocity  :: Int
              , endurance :: Int
              , cooldown  :: Int
              } deriving (Show, Eq)
data Racer    = Racer
              { deer     :: Reindeer
              , score    :: Int
              , position :: Int
              } deriving (Show, Eq)
data Race     = Race [Racer] Int
              deriving (Show)

instance Ord Racer where
    Racer { score = a } `compare` Racer { score = b } = a `compare` b

distanceRace :: String -> Int -> Int
distanceRace d t = maximum $ map (flyFor t) $ toDeer d

leadingRace :: String -> Int -> Int
leadingRace d t = score $ getWinner $ foldl' raceStep race [0..t]
    where race = (Race (map toRacer $ toDeer d) 0)
          getWinner (Race racers _) = maximum racers

raceStep :: Race -> Int -> Race
raceStep (Race racers time) tick = distPoints $ Race (map step racers) (time+1)
    where step r@(Racer { deer = d, score = s, position = pos })
              | isResting d tick = r
              | otherwise        = r { position = (pos + velocity d) }
          distPoints (Race rs n) = Race (map (award $ position $ maximumBy leader rs) rs) n
          award pos r@(Racer { position = p, score = s })
              | pos == p  = r { score = s + 1 }
              | otherwise = r

leader :: Racer -> Racer -> Ordering
leader Racer { position = a } Racer { position = b } =
    a `compare` b

isResting :: Reindeer -> Int -> Bool
isResting d t = t `mod` (endurance d + cooldown d) >= endurance d

toRacer :: Reindeer -> Racer
toRacer d = Racer { deer = d, score = 0, position = 0 }

flyFor :: Int -> Reindeer -> Int
flyFor t d = v * e * spans + v * minimum [stretch, e]
    where [v,e,c]  = [velocity d, endurance d, cooldown d]
          deerTime = e + c
          stretch  = t `mod` deerTime
          spans    = t `quot` deerTime

toDeer :: String -> [Reindeer]
toDeer = map (parseDeer . words . init) . lines
    where parseDeer [n,_,_,v,_,_,e,_,_,_,_,_,_,cd,_] =
              Reindeer { name      = n
                       , velocity  = read v
                       , endurance = read e
                       , cooldown  = read cd
                       }
