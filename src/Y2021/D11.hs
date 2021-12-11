{-|
Module:      Y2021.D11
Description: Advent of Code 2021 Day 11 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 11 set of problems for <adventofcode.com>.
-}
module Y2021.D11
  ( parse11
  , part11A
  , part11B
  , solve11A
  , solve11B
  ) where

import Data.Attoparsec.Text
import Data.Either.Utils            (fromRight)
import Data.Monoid
import Data.Text                    (Text)
import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal (rectOctGrid, RectOctGrid)
import Math.Geometry.GridMap
import Math.Geometry.GridMap.Lazy

import qualified Math.Geometry.GridMap as G

-- |A GADT is useful here; although we could wrap an `Int` it's nice to have
-- something with a bit more state
data Octopus a
  = Flashed
  | Flashing
  | Unflashed a
  deriving (Eq, Ord, Show)
-- |Type alias for better readability.
type Octopi = LGridMap RectOctGrid (Octopus Int)

-- |Solution to part A
part11A :: Text -> Int
part11A = solve11A . parse11

-- |Solve part A
solve11A :: Octopi -> Int
solve11A = getSum . fst . flip (!!) 100 . iterate octoStep . (,) (Sum 0)

-- |Solution to part B
part11B :: Text -> Int
part11B = solve11B . parse11

-- |Solve part B
solve11B :: Octopi -> Int
solve11B = go 1 . step
  where
    go n octopi
      | all isFlashed octopi = n
      | otherwise
        = go (succ n) (step $ G.map reset octopi)
    step = octoStep' . G.map succ'

-- |This is primarily for part A, which asks for iterative steps through the
-- grid but also a running sum of flashes. We could probably make it more
-- efficient with `State` to track a running sum, but this is pretty simple and
-- I know it works.
octoStep :: (Sum Int, Octopi) -> (Sum Int, Octopi)
octoStep (flashes, octopi)
  = ( flashes + foldMap flashed octopi'
    , G.map reset octopi'
    )
  where
    octopi' = octoStep' $ G.map succ' octopi
    flashed Flashed = Sum 1
    flashed _ = 0

-- |Step function to increment an `Octopus`, with the ceiling being `Flashing`
-- (we don't downgrade to `Flashed` outside of `octoStep'`)
succ' :: Octopus Int -> Octopus Int
succ' (Unflashed (succ -> n))
  | n > 9     = Flashing
  | otherwise = Unflashed n
succ' other = other

-- |After steps, we call this function on `Octopus` grid in order to turn it
-- into a clean state for the next iteration.
reset :: Octopus Int -> Octopus Int
reset Flashed  = Unflashed 0
reset Flashing = error "something went wrong!"
reset other    = other

-- |Recursive function to iterate an `Octopi` grid until the state has settled
-- and no flashing `Octopus` change the state.
octoStep' :: Octopi -> Octopi
octoStep' gOrig@(G.mapWithKey (observeFlash gOrig) -> gStepped)
  | gOrig == gStepped = gOrig
  | otherwise         = octoStep' gStepped

-- |On a given iteration, we can map the grid and test the flashes of adjacent
-- neighbors to increment the level of this `Octopus`. It's important that: we
-- increment the right number of times, and that each `Octopus` _stops_ flashing
-- after one step, which we can do with a simple pattern match.
observeFlash :: Octopi -> (Int, Int) -> Octopus Int -> Octopus Int
observeFlash _ _ Flashed  = Flashed
observeFlash _ _ Flashing = Flashed
observeFlash g point oct@(Unflashed _)
  = iterate succ' oct !! length adjacentFlashers
  where
    adjacentFlashers = [x | x <- neighbours g point, isFlashing (g ! x)]

-- |Simple predicate to check for a flashing octopus.
isFlashing :: Octopus a -> Bool
isFlashing Flashing = True
isFlashing _ = False

-- |Check whether this octopus has flashed.
isFlashed :: Octopus a -> Bool
isFlashed Flashed = True
isFlashed _ = False

-- |Parse puzzle input into a octagon rectangular grid. Need octagons in order
-- to correctly ask for diagonal neighbors.
parse11 :: Text -> Octopi
parse11 = fromRight . parseOnly (grid <$> parser)
  where
    grid [] = error "empty input"
    grid rows@(row:_) =
      lazyGridMap (rectOctGrid (length row) (length rows)) (concat rows)
    parser = line `sepBy1` endOfLine <* atEnd
    line = many1 (Unflashed . read . (: []) <$> digit)
