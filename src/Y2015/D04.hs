module Y2015.D04 (crack) where

import           Crypto.Hash.MD5 (hash)
import           Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Char8 (ByteString, pack, takeWhile)
import           Data.Monoid ((<>))

crack :: ByteString -> Int -> Int
crack prefix d = head $ dropWhile (not . validSuffix) [0..]
    where validSuffix = check . encode . hash . (prefix <>) . pack . show
          check       = (>= d) . C.length . C.takeWhile (== '0')
