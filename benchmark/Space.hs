module Main (main) where

import Weigh

import qualified Data.Text.IO as TIO
import qualified Y2021

main :: IO ()
main = do
  input <- TIO.readFile "dist/resources/2021/day8.txt"
  mainWith do
    wgroup "Y2021" do
      wgroup "D08" do
        func "Y2021.D08.A" Y2021.part8A input
