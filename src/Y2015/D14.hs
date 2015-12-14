module Y2015.D14 (reinRace) where

data Reindeer = Reindeer
              { name      :: String
              , velocity  :: Int
              , endurance :: Int
              , cooldown  :: Int
              } deriving (Show)

reinRace :: String -> Int -> Int
reinRace d i = maximum $ map (flip flyFor i) $ toDeer d

flyFor :: Reindeer -> Int -> Int
flyFor d t = v * e * spans + v * minimum [stretch, e]
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
