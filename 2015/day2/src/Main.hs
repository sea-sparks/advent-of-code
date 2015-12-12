module Main where

import Prelude hiding (length, readFile, putStr) 
import DayTwo
import Data.Text
import Data.Text.IO (readFile)
import qualified Data.Text.IO as TextIO
import System.Exit 
import System.Environment

parseFile :: String -> IO [Box]
parseFile path = do
    names <- split (=='\n') <$> (strip <$> readFile path)
    case mapM parseBox names of
        Left err -> do { putStrLn err ; exitFailure }
        Right boxen -> return boxen

usage :: [String] -> IO String
usage [path] = return $ path
usage _ = do
    putStrLn "Error: incorrect number of arguments."
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    path <- usage args
    boxen <- parseFile path
    putStrLn $ show (allBoxen boxen)