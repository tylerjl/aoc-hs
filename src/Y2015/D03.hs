module Y2015.D03 (santaRun, roboRun) where

import           Data.List   (foldl')
import           Data.Set    (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

direction :: Char -> Point
direction c | c == '^'  = (0,  1)
            | c == 'v'  = (0, -1)
            | c == '>'  = (1,  0)
            | c == '<'  = (-1, 0)
            | otherwise = (0,  0)

start :: Set Point
start = Set.singleton (0, 0)

move :: Point -> Point -> Point
move (dx, dy) (x, y) = (x + dx, y + dy)

santaRun :: String -> Int
santaRun = Set.size . deliver start . map direction

roboRun :: String -> Int
roboRun = Set.size . teamDelivery . tMap direction . divideWork
    where teamDelivery = uncurry (deliver . deliver start)

tMap :: (a -> b) -> ([a], [a]) -> ([b], [b])
tMap f (a1, a2) = (map f a1, map f a2)

divideWork :: String -> (String, String)
divideWork []  = ([],  [])
divideWork [x] = ([x], [])
divideWork (x:y:zs) = (x:xp, y:yp)
                      where (xp, yp) = divideWork zs

deliver :: Set Point -> [Point] -> Set Point
deliver = navigate (0, 0)

navigate :: Point -> Set Point -> [Point] -> Set Point
navigate origin history []          = history
navigate origin history (dir:plans) =
        let newPoint = move dir origin
            step     = Set.insert newPoint history
        in  navigate newPoint step plans
