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
-- We use mapM_ instead of mapM to throw away the list of trivial results
-- computed, as the return value is not needed, only the state.
parseString :: String -> Elevator
parseString = mapM_ parseChar

-- Run the computation represented by a value of type Elevator.
-- This happens in two steps:
--   * First, we run `flip execStateT 0`. This takes the input Elevator and
--     evaluates the State monad component of its execution, leaving only the
--     inner Exception monad.
--   * Second, we run `runExcept` on the result. This produces either an
--     error message, if the Except monad portion of the computation failed,
--     or the final state of the State monad computation.
runElevator :: Elevator -> Either String Integer
runElevator = runExcept . flip execStateT 0

-- Run a computation on a string, resulting in either an error message or
-- the floor at which Santa ended.
followInstructions :: String -> Either String Integer
followInstructions = runElevator . parseString 