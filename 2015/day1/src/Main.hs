module Main where

import System.IO 
import System.Exit 
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import DayOne (followInstructions)

fromFile :: String -> IO String
fromFile file = do
    raw <- TextIO.readFile file
    putStrLn $ "Length: " ++ show (length (Text.unpack (Text.strip raw)))
    return . Text.unpack $ Text.strip raw

fromTerm :: IO String
fromTerm = do
  putStr "Input directions: "
  hFlush stdout
  getLine

usage :: [String] -> IO (Maybe String)
usage [] = return Nothing
usage [file] = return $ Just file
usage _ = do
    putStrLn "Error: too many arguments provided."
    putStrLn "Usage: day1 [filename]"
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  result <- usage args
  instructions <- case result of
                    Nothing -> fromTerm
                    Just file -> fromFile file
  case followInstructions instructions of
    Left err -> putStrLn$ "Error: " ++ err
    Right n -> putStrLn$ show n