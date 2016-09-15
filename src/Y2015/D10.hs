{-|
Module:      Y2015.D10
Description: Advent of Code Day 10 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 10 set of problems for <adventofcode.com>.
-}

module Y2015.D10 (lookSay)  where

import Data.List (group, iterate)

-- |Play the "looksay" game
lookSay :: String -- ^ List of initial numbers
        -> Int    -- ^ Iterations
        -> String -- ^ Resulting looksay game string
lookSay = (!!) . iterate (concatMap walk . group)
    where walk s@(h:_) = show (length s) ++ [h]
