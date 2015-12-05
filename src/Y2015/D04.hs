#!/usr/bin/env runhaskell

module Y2015.D04 (mine) where

import           Codec.Binary.UTF8.String (encode, decode)
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Word (Word8)

prefix :: Int -> ByteString
prefix n = B.replicate (fromIntegral n) $ head $ encode "0"

hash :: Int -> [Word8] -> ByteString
hash i s = B.pack $ encode $ show $ md5 $ B.pack $ guess i s

guess :: Int -> [Word8] -> [Word8]
guess = flip (++) . encode . show

solve :: ByteString -> [Word8] -> Int
solve solution key = find 0
    where find i | solution `B.isPrefixOf` hash i key = i
                 | otherwise = find (i+1)

mine :: String -> Int -> Int
mine s n = solve (prefix n) (encode s)

main :: IO ()
main = do
       putStr "Part A - key is: "
       print (mine "iwrupvqb" 5)
       putStr "Part B - key is: "
       print (mine "iwrupvqb" 6)
