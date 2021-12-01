module Y2021.D01 where

import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Data.Foldable (foldl')

partA :: Text -> Int
partA = stepwise . asInts

partB :: Text -> Int
partB = stepwise . toWindows . asInts

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
