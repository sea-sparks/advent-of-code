module Main where

import System.IO

import DayOne (followInstructions)

main :: IO ()
main = do
  putStr "Input directions: "
  hFlush stdout
  instructions <- getLine
  case followInstructions instructions of
    Left err -> putStrLn$ "Error: " ++ err
    Right n -> putStrLn$ show n
