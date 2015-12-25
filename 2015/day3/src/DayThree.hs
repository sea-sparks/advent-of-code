{-# LANGUAGE ViewPatterns #-}

module DayThree where

import Prelude hiding (replicate)
import Data.Set 
import Data.Sequence hiding (take, singleton)
import Control.Monad.Except


data Loc = Loc { xCoord :: Int, yCoord :: Int } deriving (Eq, Show)

instance Ord Loc where
  compare (Loc x1 y1) (Loc x2 y2) =
      case (compare x1 x2, compare y1 y2) of
        (EQ, yc) -> yc 
        (xc, _)  -> xc 

data Houses = Houses { santas :: Seq Loc
                     , visited :: Set Loc
                     }

start :: Int -> Houses
start n = Houses (replicate n $ Loc 0 0) (singleton $ Loc 0 0)

data Dir = LEFT | RIGHT | UP | DOWN 

dir :: Dir -> Loc -> Loc 
dir LEFT  (Loc x y) = Loc (x-1) y
dir RIGHT (Loc x y) = Loc (x+1) y
dir UP    (Loc x y) = Loc x (y+1)
dir DOWN  (Loc x y) = Loc x (y-1)

go :: Dir -> Houses -> Houses
go d (Houses (viewl -> loc :< locs) vis) =
  Houses (locs |> dir d loc) (insert (dir d loc) vis)

parseArrow :: Char -> Either String (Houses -> Houses)
parseArrow '<' = return $ go LEFT
parseArrow '>' = return $ go RIGHT
parseArrow 'v' = return $ go DOWN
parseArrow '^' = return $ go UP
parseArrow c   = throwError $ "Error: " ++ [c] ++ " is not a valid direction"

followDirections :: Int -> [Char] -> Either String Houses
followDirections n = foldM (\houses c -> parseArrow c <*> return houses) $ start n