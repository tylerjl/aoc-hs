#!/usr/bin/env runhaskell

module Y2015.D03 (solve, roboSolve) where

import           Control.Monad
import           Control.Arrow
import           Data.Bifunctor
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

direction :: Char -> Point
direction c | c == '^'  = (0,  1)
            | c == 'v'  = (0, -1)
            | c == '>'  = (1,  0)
            | c == '<'  = (-1, 0)
            | otherwise = (0,  0)

move :: Point -> Point -> Point
move (dx, dy) (x, y) = (x + dx, y + dy)

solve :: String -> Int
solve = Map.size . deliver . map direction

roboSolve :: String -> Int
roboSolve = Map.size . teamDelivery . tMap direction . divideWork
    where teamDelivery (santa, robot) = navigate (0, 0) (deliver santa) robot

tMap :: (a -> b) -> ([a], [a]) -> ([b], [b])
tMap f (a1, a2) = (map f a1, map f a2)

divideWork :: String -> (String, String)
divideWork []  = ([],  [])
divideWork [x] = ([x], [])
divideWork (x:y:zs) = (x:xp, y:yp)
                      where (xp, yp) = divideWork zs

deliver :: [Point] -> Map Point Int
deliver = navigate (0, 0) (Map.singleton (0, 0) 1)

navigate :: Point -> Map Point Int -> [Point] -> Map Point Int
navigate origin history []          = history
navigate origin history (dir:plans) =
        let newPoint = move dir origin
        in
            navigate newPoint (Map.insertWith record newPoint 1 history) plans
        where
            record new old = new + old

main :: IO ()
main = do
       input <- readFile "src/Y2015/D03_input"
       putStrLn "Part A: delivery is: "
       print (solve input)
       putStrLn "Part B: total number delivered as a team is: "
       print (roboSolve input)
