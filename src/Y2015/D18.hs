{-|
Module:      Y2015.D18
Description: Advent of Code Day 18 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the day 18 set of problems for <adventofcode.com>.
-}

{-# LANGUAGE QuasiQuotes #-}

module Y2015.D18 (animateLights, animateStuckLights) where

import           Data.Array.Repa              ((:.)(..), Array, DIM2, U, Z(..))
import qualified Data.Array.Repa as           R
import           Data.Array.Repa.Stencil      (Boundary(..), Stencil)
import           Data.Array.Repa.Stencil.Dim2 (makeStencil2, mapStencil2, stencil2)
import           Data.Bits                    ((.|.), Bits(..))
import           Data.Vector.Unboxed.Base     (Unbox)

type Lights a = Array U DIM2 a

-- |Animates an example light grid
animateLights :: String -- ^ Raw string representing starting state
              -> Int    -- ^ Number of cycles to let animations repeat
              -> Int    -- ^ Number of lit lights
animateLights s n = R.sumAllS $ iterate animate (initialGrid s) !! n

-- |Animates an example light grid with stuck lights
animateStuckLights :: String -- ^ Raw string representing starting state
                   -> Int    -- ^ Number of cycles to let animations repeat
                   -> Int    -- ^ Number of lit lights
animateStuckLights s n = R.sumAllS $ iterate (stuck e . animate) g' !! n
    where g  = initialGrid s
          e  = R.extent g
          g' = stuck e g

stuck :: (Bits a, Num a, Unbox a) => R.DIM2 -> Lights a -> Lights a
stuck e = R.computeS . R.zipWith (.|.) (stuckLights e)

stuckLights :: (Num a, Unbox a) => R.DIM2 -> Lights a
stuckLights sh = R.fromListUnboxed sh [corner x | x <- [1..s]]
    where s      = R.size sh
          i      = truncate ((sqrt $ fromIntegral s) :: Double)
          corner 1 = 1
          corner n | n == i           = 1
                   | n == s           = 1
                   | n == (s - i) + 1 = 1
                   | otherwise        = 0

animate :: Lights Int -> Lights Int
animate grid = R.computeS $ R.zipWith step grid adjacent
    where adjacent = mapStencil2 (BoundConst 0) stencil grid

step :: Int -> Int -> Int
step 1 2 = 1
step 1 3 = 1
step 0 3 = 1
step _ _ = 0

stencil :: Stencil DIM2 Int
stencil = [stencil2| 1  1  1
                     1  0  1
                     1  1  1 |]

initialGrid :: (Num a, Unbox a) => String -> Lights a
initialGrid s = R.fromListUnboxed (Z :. size :. size :: R.DIM2) lights
    where scrubbed    = filter (/= '\n') s
          size        = truncate ((sqrt $ fromIntegral $ length scrubbed) :: Double)
          lights      = map toLight scrubbed
          toLight '#' = 1
          toLight  _  = 0
