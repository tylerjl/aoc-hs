#!/usr/bin/env runhaskell

module Y2015.D11 (rotate) where

import Y2015.Util ((<&&>))
import Data.List (elemIndex, foldr, group, isInfixOf, iterate, tails)

alphabet :: String
alphabet = ['a'..'z']

meetsReqs :: String -> Bool
meetsReqs = hasPairs <&&> (not . forbidden) <&&> hasStraight

hasPairs :: String -> Bool
hasPairs = (1 <) . length . filter ((<) 1 . length) . group

forbidden :: String -> Bool
forbidden = any (`elem` "iol")

hasStraight :: String -> Bool
hasStraight = not . null . filterAsc . subSeqs
    where filterAsc = filter (`isInfixOf` alphabet)
          subSeqs   = takeWhile ((== 3) . length) . map (take 3) . tails

increment :: String -> String
increment = reverse . step . reverse
    where step []                 = []
          step [x]    | x == 'z'  = "aa"
                      | otherwise = [succ x]
          step (x:xs) | x /= 'z'  = succ x :      xs
                      | otherwise = 'a'    : step xs

rotate :: String -> String
rotate = nextValid . increment
    where nextValid = head . filter meetsReqs . iterate (increment)

main :: IO ()
main = do
    putStr "Part A - next best password is: "
    let pw  = "hepxcrrq"
        pw1 = rotate pw
    print pw1
    putStr "Part B - next best password is: "
    print $ rotate pw1
