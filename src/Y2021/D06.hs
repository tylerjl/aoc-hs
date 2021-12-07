{-# LANGUAGE FlexibleContexts #-}
{-|
Module:      Y2021.D06
Description: Advent of Code 2021 Day 06 Solutions.
License:     MIT
Maintainer:  @tylerjl

Solutions to the 2021 day 06 set of problems for <adventofcode.com>.
-}
module Y2021.D06 where

import Data.Either.Utils (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (Empty)
import Y2015.Util (regularParse', intParser')

import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (runST)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import Data.List (elemIndices)
import Witch
import qualified Data.Attoparsec.Text as AP

-- |Solve part A
part6A :: Text -> Int
part6A = solve6 80 . parseFish

-- |Solve part A, with unboxed mutable vectors
part6AMV :: Text -> Int
part6AMV = solve6MV 80 . parseFish

-- |Solve part A, with sequences
part6ASeq :: Text -> Int
part6ASeq = solve6Seq 80 . Seq.fromFunction 9 . build . parseFish
  where build fish n = length $ elemIndices n fish

-- |Solve part B
part6B :: Text -> Int
part6B = solve6 256 . parseFish

-- |Solve part B, with unboxed mutable vectors
part6BMV :: Text -> Int
part6BMV = solve6MV 256 . parseFish

-- |Solve part B, with sequences
part6BSeq :: Text -> Int
part6BSeq = solve6Seq 256 . Seq.fromFunction 9 . build . parseFish
  where build fish n = length $ elemIndices n fish

solve6Seq :: Int -> Seq Int -> Int
solve6Seq n fish = Seq.foldlWithIndex sum' 0 $ iterate solve6Seq' fish !! n
  where sum' acc _ el = acc + el

solve6Seq' :: Seq Int -> Seq Int
solve6Seq' (a:<|b:<|c:<|d:<|e:<|f:<|g:<|h:<|i:<|Empty)=
  b:<|c:<|d:<|e:<|f:<|g:<|h+a:<|i:<|a:<|Empty
solve6Seq' _ = error "unexpected fish sequence size"

-- |Solution algorithm using super aggressive mutable unboxed vectors.
solve6MV :: Int -> [Int] -> Int
solve6MV days fish = runST $ do
  vec <- V.replicate 9 0
  forM_ fish $ \fish' -> do
    V.modify vec (+ 1) fish'
  replicateM_ days $ do
    newFish <- V.read vec 0
    V.move (V.slice 0 8 vec) (V.slice 1 8 vec)
    V.write vec 8 newFish
    V.modify vec (+ newFish) 6
  V.foldl' (+) 0 vec

-- |Iterate for a given number of days given a starting value and return the
-- total.
solve6 :: Int -> [Int] -> Int
solve6 n
  = M.foldl' (+) 0
  . flip (!!) n
  . iterate breed
  . M.fromListWith (+)
  . flip zip (repeat 1)

-- |Simulate one day passing for the collection of fish.
breed :: Num a => M.IntMap a -> M.IntMap a
breed fish =
  M.insert 8 newFish $
  M.mapKeysWith (+) step fish
  where
    step (pred -> age) | age < 0   = 6
                       | otherwise = age
    newFish = M.findWithDefault 0 0 fish

-- |Parse puzzle input into a list of `Int`s.
parseFish :: Text -> [Int]
parseFish = fromRight . regularParse' fishParser
  where fishParser = intParser' `sepBy1` char ',' <* newline <* eof

-- |Parse puzzle input into a list of `Int`s but do so with a dumb parser.
parseFish' :: Text -> [Int]
parseFish' = map (read . into @String) . T.splitOn ","

-- |Parse puzzle input into a list of `Int`s but do so with a dumb parser.
parseFish'' :: Text -> [Int]
parseFish'' = fromRight . AP.eitherResult . AP.parse parser
  where parser = AP.decimal `AP.sepBy` AP.char ',' <* AP.endOfLine
