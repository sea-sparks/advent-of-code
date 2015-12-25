module Main where

import DayThree

import Control.Monad.State
import System.IO 
import System.Exit 
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Set (size)
import Text.Read (readMaybe)


usage :: [String] -> IO (Maybe (String, Int))
usage [] = return Nothing
usage [file] = return $ Just (file, 1)
usage [file, n] = do
    case readMaybe n of
      Just n -> return $ Just (file, n)
      Nothing -> do
        putStrLn "Error: second argument must be a positive integer"
        putStrLn "Usage: day3 [filename [numSantas]]"
        exitFailure
usage _ = do
    putStrLn "Error: too many arguments provided."
    putStrLn "Usage: day3 [filename [numSantas]]"
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
  (dirs, n) <- case result of
            Nothing -> flip (,) 1 <$> fromTerm
            Just (file, n) -> flip (,) n <$> fromFile file
  case followDirections n dirs of
    Left err -> putStrLn err
    Right (Houses _ vis) -> putStrLn$ "Houses visited: " ++ show (size vis)