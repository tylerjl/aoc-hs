module Main (main) where

import Weigh

import qualified Data.Text.IO as TIO
import qualified Y2021

main :: IO ()
main = do
  d8 <- TIO.readFile "dist/resources/2021/day8.txt"
  d9 <- TIO.readFile "dist/resources/2021/day9.txt"
  mainWith do
    wgroup "Y2021" do
      wgroup "D08" do
        func "Y2021.D08.A" Y2021.part8A d8
        func "Y2021.D08.B" Y2021.part8B d8
      wgroup "D09" do
        func "Y2021.D09.A" Y2021.part9A d9
        func "Y2021.D09.B" Y2021.part9B d9
