{-|
Module:      Y2021
Description: Advent of Code Day Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the set of problems for <adventofcode.com>.

Each day is broken up into an individual module with accompaying spec tests
and (possibly?) benchmarks.
Each day's module exports are re-exported here for convenience when importing
'Y2021' et. al.

This year I leaned a little bit more into making things very "Haskell"-ly by relying more on laziness, centralizing solutions around data types, etc.
-}

module Y2021 (module X) where

import Y2021.D01 as X
import Y2021.D02 as X
import Y2021.D03 as X
import Y2021.D04 as X
import Y2021.D05 as X
import Y2021.D06 as X
