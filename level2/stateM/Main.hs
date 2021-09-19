module Main where

import Control.Monad.State.Lazy

---------- Using the defined State type below

-- newtype State s a = State {runState :: s -> (a, s)}

-- -- look at our counter and return "foo" or "bar"
-- -- along with the incremented counter:
-- fromStoAandS :: Int -> (String, Int)
-- fromStoAandS c
--   | c `mod` 5 == 0 = ("foo", c + 1)
--   | otherwise = ("bar", c + 1)

-- stateIntString :: State Int String
-- stateIntString = State fromStoAandS

-- runState stateIntString 1
------------

data GameState = GameState {gsSecret :: Int, gsWon :: Bool, gsGuessAttempt :: Int, gsAttempts :: [Int]} deriving (Show)

checkGuess :: Int -> State GameState Bool
checkGuess guess = do
  s <- get
  let won = gsSecret s == guess
      pa = guess : gsAttempts s
  put $ GameState (gsSecret s) won (gsGuessAttempt s + 1) pa
  pure won

-- λ> import Main
-- λ> runState (checkGuess 2) $ GameState 5 False 0

guessGame :: State GameState Bool
guessGame = do
  checkGuess 1
  checkGuess 2
  checkGuess 5

tick :: State Int Int
tick = do
  n <- get
  put (n + 1)
  return n

main :: IO ()
main = print ""
