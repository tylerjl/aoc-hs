module Y2021.D01 where

import Data.Foldable (foldl')
import Data.Text (Text)
import Witch

import qualified Data.Text as T

partA :: Text -> Int
partA = stepwise . asInts

partAZip :: Text -> Int
partAZip = length . compareAdj . asInts

partARecur :: Text -> Int
partARecur = go . asInts
  where go (x:y:zs) = (if x < y then 1 else 0) + go (y:zs)
        go _ = 0

partB :: Text -> Int
partB = stepwise . toWindows . asInts

partBZip :: Text -> Int
partBZip = length . compareAdj . map trisum . (zip3 <*> tail <*> tail . tail) . asInts

compareAdj :: [Int] -> [(Int, Int)]
compareAdj = filter (uncurry (<)) . (zip <*> tail)

trisum :: Num a => (a, a, a) -> a
trisum (a, b, c) = a + b + c

stepwise :: [Int] -> Int
stepwise = snd . foldl' steps (Nothing, 0)
  where steps :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
        steps (Nothing,   acc) n = (Just n, acc)
        steps (Just prev, acc) n
          | prev < n  = (Just n, succ acc)
          | otherwise = (Just n, acc)

toWindows :: [Int] -> [Int]
toWindows (w:x:y:zs) = (w+x+y) : toWindows (x:y:zs)
toWindows _ = []

asInts :: Text -> [Int]
asInts = map (read . into @String) . T.lines
