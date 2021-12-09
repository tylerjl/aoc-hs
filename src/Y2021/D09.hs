{-|
Module:      Y2021.D09
Description: Advent of Code 2021 Day 09 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 09 set of problems for <adventofcode.com>.
-}
module Y2021.D09
  ( part9A
  , part9B
  , parse9
  )
  where

import Data.Attoparsec.Text hiding (take)
import Data.Either.Utils (fromRight)
import Data.List (group, sort, sortOn)
import Data.List.Extra (nubOrdOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down(Down))
import Data.Text (Text)
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Math.Geometry.GridMap hiding (map)
import Math.Geometry.GridMap.Lazy (lazyGridMap, LGridMap)

-- |Type alias for better readability.
type Point = (Int, Int)
-- |Type alias for better readability.
type SeaFloor = LGridMap RectSquareGrid Int
-- |Type alias for better readability.
type Basins = LGridMap RectSquareGrid (Maybe (Point, Int))

-- |Solve part A
part9A :: Text -> Int
part9A = sum . map (succ . snd) . nubOrdOn fst . findBasins . parse9

-- |Solve part B
part9B :: Text -> Int
part9B =
  product .
  take 3 .
  sortOn Down . map length . group . sort . map fst . findBasins . parse9

-- |Some common glue between our mapping function and extracting the values
-- we're interested in.
findBasins :: SeaFloor -> [(Point, Int)]
findBasins = catMaybes . elems . basins

-- |Recursive map over `SeaFloor` that turns each grid point into a possible
-- representation of the basin this point flows to. `Grid` takes the brunt of
-- the boilterplate here with `neighbours` and `mapWithKey`.
basins :: SeaFloor -> Basins
basins g = mapWithKey toBasins g
  where
    -- Although `basins` is the top-level map, toBasins is what we'll
    -- recursively call when we find a point that has to map to a low point
    -- somewhere.
    toBasins point value
      | value == 9      = Nothing
      | []  <- adjacent = Just (point, value)
      | otherwise       = minimum (map (uncurry toBasins) adjacent)
      where
        adjacent = [(x, g ! x) | x <- neighbours g point, g ! x < value]

-- |Parse puzzle input into a `Grid`. I could probably do the conversion from
-- `[[Int]]` to `Grid outside of the parser, but it's nice to go directly to the
-- main data structure for the problem.
parse9 :: Text -> SeaFloor
parse9 = fromRight . parseOnly (grid <$> parser)
  where
    grid [] = error "empty input"
    grid rows@(row:_) =
      lazyGridMap (rectSquareGrid (length row) (length rows)) (concat rows)
    parser = line `sepBy1` endOfLine <* atEnd
    line = many1 (read . (: []) <$> digit)
