module Main where

import DayFive

import System.IO 
import System.Exit 
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

usage :: [String] -> IO String
usage [file] = return file
usage _ = do
    putStrLn "Error: wrong number of arguments"
    putStrLn "Usage: day5 filename"
    exitFailure

fromFile :: String -> IO String
fromFile file = do
    raw <- TextIO.readFile file
    return $ Text.unpack $ Text.strip raw

main :: IO ()
main = do
  args <- getArgs
  filename <- usage args
  strings <- lines <$> fromFile filename
  let niceWords = filter (\s -> all ($s) props2) strings
  putStrLn $ "Nice words:"
  mapM_ putStrLn niceWords
  putStrLn $ ""
  putStrLn $ "Number of nice words: " ++ show (countNice props2 strings)