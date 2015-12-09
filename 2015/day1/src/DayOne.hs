module DayOne where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Arrow 

newtype MinInteger = MinInteger {fromMinInteger :: Integer} deriving (Show, Eq, Ord)

instance Monoid MinInteger where
    mempty = MinInteger 0
    mappend (MinInteger 0) (MinInteger i2) = MinInteger i2
    mappend (MinInteger i1) (MinInteger 0) = MinInteger i1
    mappend (MinInteger i1) (MinInteger i2) = MinInteger (min i1 i2)

accumulate :: (Monad m) => (a -> m b -> m b) -> m b -> [a] -> m b
accumulate f m [] = m 
accumulate f m (x : xs) = f x (accumulate f m xs) 

-- We are keeping track of three things in monads:
--   * A possible error state
--   * An Integer representing which floor we are on
--   * An Integer representing the index of the first instruction on which
--     the elevator travels to the basement, or 0 if this never happens.
-- To represent this, we nest writer and state monads within an
-- error monad, since the error is the top-level effect.
type Elevator = RWST Integer MinInteger Integer (Except String) ()

-- Helper functions to increment and decrement the value stored
-- in the State monad.
incr :: Elevator 
incr = do 
    x <- get
    put$ x+1

decr :: Elevator 
decr = do 
    x <- get
    when (x == 0) (do { index <- ask ; tell $ MinInteger index })
    put $ x-1

-- Parse a single character. The '(' character means go down a floor and
-- should result in the stored state being decremented; the ')' character
-- means go up a floor and should result in the stored state being
-- incremented; anything else is an error.
parseChar :: Char -> Elevator
parseChar '(' = incr
parseChar ')' = decr 
parseChar c   = throwError$ "Unrecognized character " ++ [c]

-- Parse an entire string of characters one at a time.
-- We use mapM_ instead of mapM to throw away the list of trivial results
-- computed, as the return value is not needed, only the state.
parseString :: String -> Elevator
parseString = accumulate (\char rest -> do { parseChar char ; local (+1) rest })
                         (return ())

-- Run the computation represented by a value of type Elevator.
-- This happens in two steps:
--   * First, we run `flip execStateT 0`. This takes the input Elevator and
--     evaluates the State monad component of its execution, leaving only the
--     inner Exception monad.
--   * Second, we run `runExcept` on the result. This produces either an
--     error message, if the Except monad portion of the computation failed,
--     or the final state of the State monad computation.
runElevator :: Elevator -> Either String (Integer, MinInteger)
runElevator e = runExcept $ execRWST e 1 0

-- Run a computation on a string, resulting in either an error message or
-- the floor at which Santa ended.
followInstructions :: String -> Either String (Integer, MinInteger)
followInstructions = runElevator . parseString 