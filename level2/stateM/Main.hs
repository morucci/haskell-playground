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

-- tick :: State Int Int
-- tick = do
--   n <- get
--   put (n + 1)
--   return n

data GameState = GameState {gsSecret :: Int, gsWon :: Bool, gsGuessAttempt :: Int, gsAttempts :: [Int]} deriving (Show)

checkGuess :: Int -> State GameState Int
checkGuess guess = do
  s <- get
  let won = gsSecret s == guess
      pa = guess : gsAttempts s
  put $ GameState (gsSecret s) won (gsGuessAttempt s + 1) pa
  pure guess

-- λ> runState (checkGuess 2) $ GameState 5 False 0

-- With the state monad
guessGame :: State GameState Int
guessGame = do
  checkGuess 1
  checkGuess 2
  checkGuess 5

-- λ> runState guessGame $ GameState 5 False 0 []

type AppM = StateT GameState IO ()

runGame :: AppM
runGame = game
  where
    game :: AppM
    game = do
      s <- get
      if not $ gsWon s
        then round >> game
        else do
          liftIO . print $ "Game ended with state:" <> show s
          pure ()
    round :: AppM
    round = do
      liftIO $ putStrLn "Enter a guess"
      guessS <- liftIO getLine
      let guess = read guessS :: Int
      s <- get
      let won = gsSecret s == guess
          pa = guess : gsAttempts s
      put $ GameState (gsSecret s) won (gsGuessAttempt s + 1) pa
      pure ()

main :: IO ()
main = print "Test"
