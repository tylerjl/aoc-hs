module Y2015.D25 (manualCodeFrom) where

type Point = (Int, Int)

manualCodeFrom :: String -> Integer
manualCodeFrom = (!!) manualSeq . toPos . toPoint
    where toPos (x, y) = (i - 1) * (i - 2) `div` 2 + i - x - 1
                       where i = x + y

toPoint :: String -> Point
toPoint = go . words
    where go s = (grab 15, grab 17)
                 where grab = read . init . (!!) s

manualSeq :: [Integer]
manualSeq = iterate f 20151125 where
    f = flip mod 33554393 . (*) 252533
