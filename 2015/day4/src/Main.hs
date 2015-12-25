module Main where

import DayFour

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
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
    putStrLn "Usage: day4 [filename]"
    exitFailure

fromFile :: String -> IO ByteString
fromFile file = do
    raw <- BS.readFile file
    return . head $ BS.lines raw

fromTerm :: IO ByteString
fromTerm = do
  putStr "Input key: "
  hFlush stdout
  head . BS.lines <$> BS.getLine

main :: IO ()
main = do
  args <- getArgs
  result <- usage args
  key <- case result of
    Nothing -> fromTerm
    Just file -> fromFile file
  putStrLn $ "First value with five zeroes: " ++ show (findFiveZeroes key)
  putStrLn $ "First value with six zeroes: " ++ show (findSixZeroes key)