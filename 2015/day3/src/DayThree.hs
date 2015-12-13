module DayThree where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Class

data HouseRow = HouseRow {left :: [Integer], here :: Integer, right :: [Integer]}
data Houses = Houses {down :: [HouseRow],  center :: HouseRow, up :: [HouseRow]}

startRow :: HouseRow
startRow = HouseRow (repeat 0) 1 (repeat 0)

otherRow :: HouseRow 
otherRow = HouseRow (repeat 0) 0 (repeat 0)

start :: Houses
start = Houses (repeat otherRow) startRow (repeat otherRow)

goLeftRow :: HouseRow -> HouseRow
goLeftRow (HouseRow (l : ls) h r) = HouseRow ls l (h : r)

goRightRow :: HouseRow -> HouseRow
goRightRow (HouseRow l h (r : rs)) = HouseRow (h : l) r rs

goLeft :: Houses -> Houses
goLeft (Houses d c u) = Houses (map goLeftRow d) (goLeftRow c) (map goLeftRow u)

goRight :: Houses -> Houses
goRight (Houses d c u) = Houses (map goRightRow d) (goRightRow c) (map goRightRow u)

goUp :: Houses -> Houses
goUp (Houses (u : us) c d) = Houses us u (c : d)

goDown :: Houses -> Houses
goDown (Houses u c (d : ds)) = Houses (c : u) d ds

visitRow :: (Monad m) => (Integer -> m Integer) -> HouseRow -> m HouseRow
visitRow f (HouseRow l h r) = do 
  h' <- f h
  return $ HouseRow l h' r 

visit :: (Monad m) => (Integer -> m Integer) -> Houses -> m Houses
visit f (Houses d c u) = do 
  c' <- visitRow f c 
  return $ Houses d c' u 

parseArrow :: Char -> Either String (Houses -> Houses)
parseArrow '<' = return goLeft 
parseArrow '>' = return goRight 
parseArrow 'v' = return goDown
parseArrow '^' = return goUp
parseArrow c   = throwError $ "Error: " ++ [c] ++ " is not a valid direction"

incr :: (Num s, MonadState s m) => m ()
incr = do 
  x <- get
  put$ x+1

deliver :: Integer -> StateT Integer (Either String) Integer
deliver n = do 
  when (n == 0) incr
  return $ n+1

followDirections :: [Char] -> StateT Integer (Either String) Houses
followDirections = flip followDirections' start 

followDirections' :: [Char] -> Houses -> StateT Integer (Either String) Houses
followDirections' [] houses = do
  return houses
followDirections' (c : cs) houses = do
  dir <- lift $ parseArrow c
  houses' <- visit deliver (dir houses)
  followDirections' cs houses' 





