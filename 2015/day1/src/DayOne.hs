module DayOne where

import Control.Monad.Except
import Control.Monad.State 

-- We are keeping track of two things in monads:
--   * A possible error state
--   * An Integer representing which floor we are on
-- To represent this, we nest a state monad within an
-- error monad, since the error is the top-level effect.
type Elevator = StateT Integer (Except String) ()

-- Helper functions to increment and decrement the value stored
-- in the State monad.
incr :: (Num s, MonadState s m) => m ()
incr = do 
    x <- get
    put$ x+1

decr :: (Num s, MonadState s m) => m ()
decr = do 
    x <- get
    put$ x-1

-- Parse a single character. The '(' character means go down a floor and
-- should result in the stored state being decremented; the ')' character
-- means go up a floor and should result in the stored state being
-- incremented; anything else is an error.
parseChar :: Char -> Elevator
parseChar '(' = decr
parseChar ')' = incr
parseChar c   = throwError$ "Unrecognized character " ++ [c]

-- Parse an entire string of characters one at a time.
parseString :: String -> Elevator
parseString = mapM_ parseChar

-- Run the computation represented by a value of type Elevator
runElevator :: Elevator -> Either String Integer
runElevator = runExcept . flip execStateT 0

-- Run a computation on a string, resulting in either an error message or
-- the floor at which Santa ended.
followInstructions :: String -> Either String Integer
followInstructions = runElevator . parseString 