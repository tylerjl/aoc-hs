module Main (main) where

import AoC
import Chart
import Weigh
import Witch

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  d8 <- TIO.readFile "dist/resources/2021/day8.txt"
  d9 <- TIO.readFile "dist/resources/2021/day9.txt"
  d11 <- TIO.readFile "dist/resources/2021/day11.txt"
  (measurements, config) <- weighResults $ do
    wgroup "Y2021" do
      wgroup "D08" do
        func "Y2021.D08.A" (solve 2021 8 'a') d8
        func "Y2021.D08.B" (solve 2021 8 'b') d8
      wgroup "D09" do
        func "Y2021.D09.A" (solve 2021 9 'a') d9
        func "Y2021.D09.B" (solve 2021 9 'b') d9
      wgroup "D011" do
        func "Y2021.D011.A" (solve 2021 11 'a') d11
        func "Y2021.D011.B" (solve 2021 11 'b') d11

  let (labels, allocs, gcs) = unzip3 $ flip concatMap measurements \(Grouped year days) -> concatMap sumData days
      points = zipWith Point allocs (map into gcs)
      graphs = zip labels points
  print graphs

  where
    sumData (Grouped day stats) = map sumDay stats
    sumData (Singleton _) = error "no benchmarks for bare singles"
    sumDay (Singleton (Weight{..}, _)) = (weightLabel, weightAllocatedBytes, weightGCs)
    sumDay (Grouped _ _) = error "should be a day group"
