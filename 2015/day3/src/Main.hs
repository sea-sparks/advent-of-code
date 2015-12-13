module Main where

import DayThree
import Control.Monad.State
import System.IO 
import System.Exit 
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

usage :: [String] -> IO (Maybe String)
usage [] = return Nothing
usage [file] = return $ Just file
usage _ = do
    putStrLn "Error: too many arguments provided."
    putStrLn "Usage: day1 [filename]"
    exitFailure

fromFile :: String -> IO String
fromFile file = do
    raw <- TextIO.readFile file
    return $ Text.unpack $ Text.strip raw

fromTerm :: IO String
fromTerm = do
  putStr "Input directions: "
  hFlush stdout
  getLine

main :: IO ()
main = do
  args <- getArgs
  result <- usage args
  dirs <- case result of
            Nothing -> fromTerm
            Just file -> fromFile file
  case execStateT (followDirections dirs) 1 of
    Left err -> putStrLn err
    Right vis -> putStrLn$ "Houses visited: " ++ show vis