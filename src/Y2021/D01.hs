module Y2021.D01 where

import Data.Text (Text)
import qualified Data.Text as T
import Witch
import Data.Foldable (foldl')

partA :: Text -> Int
partA =
  snd
  . foldl' steps (Nothing, 0)
  . map (read . into @String)
  . T.lines
  where steps :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
        steps (Nothing,   acc) n = (Just n, acc)
        steps (Just prev, acc) n
          | prev < n  = (Just n, succ acc)
          | otherwise = (Just n, acc)
