module Y2015.D18 (animateLights) where

{-# LANGUAGE QuasiQuotes #-}

import           Data.Array.Repa              ((:.)(..), Array, DIM2, U, Z(..))
import qualified Data.Array.Repa as           R
import           Data.Array.Repa.Stencil      (Boundary(..), Stencil)
import           Data.Array.Repa.Stencil.Dim2 (makeStencil2, mapStencil2, stencil2)
import           Data.Vector.Unboxed.Base     (Unbox)

type Lights a = Array U DIM2 a

animateLights :: String -> Int -> Int
animateLights s n = R.sumAllS $ iterate animate (initialGrid s) !! n

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
                     1  1  1|]

initialGrid :: (Num a, Unbox a) => String -> Lights a
initialGrid s = R.fromListUnboxed (Z :. size :. size :: R.DIM2) lights
    where scrubbed    = filter (/= '\n') s
          size        = truncate $ sqrt $ fromIntegral $ length scrubbed
          lights      = map toLight scrubbed
          toLight '#' = 1
          toLight  _  = 0
