module DayFour where

import Debug.Trace (trace)

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (append, pack, isPrefixOf)
import Data.ByteString (ByteString)
import Data.Hex (hex)

natsAsByteStrings :: [ByteString]
natsAsByteStrings = map (pack . show) [1..]

findFirstNPrefixes :: Int -> ByteString -> ByteString -> [ByteString]
findFirstNPrefixes = nPrefixes natsAsByteStrings []

nPrefixes :: [ByteString] -> [ByteString] -> Int -> ByteString -> ByteString -> [ByteString]
nPrefixes _ rets 0 _ _ = reverse rets
nPrefixes (x : xs) rets n prefix key =
  let input = key `append` x 
      hval  = hash input
  in
    if prefix `isPrefixOf` (hex $ hash (key `append` x))
    then nPrefixes xs (x : rets) (n-1) prefix key
    else nPrefixes xs rets n prefix key

findFiveZeroes :: ByteString -> ByteString
findFiveZeroes key = head $ findFirstNPrefixes 1 (pack "00000") key

findSixZeroes :: ByteString -> ByteString
findSixZeroes key = head $ findFirstNPrefixes 1 (pack "000000") key