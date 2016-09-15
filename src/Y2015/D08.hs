{-|
Module:      Y2015.D08
Description: Advent of Code Day 08 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 08 set of problems for <adventofcode.com>.
-}

module Y2015.D08 (difference, encoded) where

body :: [a] -> [a]
body []  = []
body [x] = []
body xs  = tail $ init xs

hexChars :: String
hexChars = "0123456789abcdef"

escape :: String -> String
escape []                                   = []
escape [x]                                  = [x]
escape (v:w:x:y:zs) | [v,w] == "\\x" && hex = '.'  : escape zs
                    where hex = all (`elem` hexChars) [x,y]
escape (x:y:zs)     | [x,y] == "\\\""       = '"'  : escape zs
                    | [x,y] == "\\\\"       = '\\' : escape zs
                    | otherwise             = x    : escape (y:zs)

encode :: String -> String
encode = (++) "\"" . (:) '"' . encode'

encode' :: String -> String
encode' []                 = []
encode' (x:xs) | x == '"'  = '\\' : '"' : encode' xs
               | x == '\\' = "\\\\"    ++ encode' xs
               | otherwise = x          : encode' xs

squish :: String -> String
squish = filter (/= '\n')

solve :: (Int -> Int -> Int) -> (String -> String) -> String -> Int
solve g f s = g (original s) (new s)
    where original = length . squish
          new      = length . squish . unlines . map f . lines

-- |Finds the length difference between escaped and unescaped string
difference :: String -- ^ Input string with escapes
           -> Int    -- ^ Unescaped length difference
difference = solve (-) (escape . body)

-- |Same as 'encoded', but different
encoded :: String -- ^ Input string with escapes
        -> Int    -- ^ Length difference to encoded string
encoded = solve (flip (-)) encode
