module DayFive where

import Data.List (isInfixOf, isPrefixOf)

props :: [String -> Bool]
props = [
    \s -> length (filter (`elem` "aeiou") s) >= 3
  , \s -> length s > 0 && (any id $ zipWith (==) s (tail s))
  , not . (isInfixOf "ab")
  , not . (isInfixOf "cd")
  , not . (isInfixOf "pq")
  , not . (isInfixOf "xy")
  ]

redactFirst :: (Eq a) => a -> [a] -> [a] -> [a]
redactFirst r xs ys | xs `isPrefixOf` ys = replicate (length xs) r ++ drop (length xs) ys
redactFirst r xs (y : ys) = y : redactFirst r xs ys
redactFirst r xs [] = []

byPairs :: [a] -> [[a]]
byPairs [] = []
byPairs [x] = []
byPairs (x : y : xs) = [x, y] : byPairs (y : xs)

repeatAfterOne :: (Eq a) => [a] -> Bool
repeatAfterOne (a : b : c : rest) = a == c || repeatAfterOne (b : c : rest)
repeatAfterOne _ = False

props2 :: [String -> Bool]
props2 = [
    \s -> any (\pair -> pair `isInfixOf` (redactFirst '0' pair s)) (byPairs s)
  , repeatAfterOne
  ]

nice :: [String -> Bool] -> String -> Bool
nice ps s = all ($s) ps

countNice :: [String -> Bool] -> [String] -> Int
countNice ps = length . filter (nice ps) 