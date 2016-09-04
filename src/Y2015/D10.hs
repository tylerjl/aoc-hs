module Y2015.D10 (lookSay)  where

import Data.List (group, iterate)

lookSay :: String -> Int -> String
lookSay = (!!) . iterate (concatMap walk . group)
    where walk s@(h:_) = show (length s) ++ [h]
