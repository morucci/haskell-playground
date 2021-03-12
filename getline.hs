-- main = do
--   a <- getLine
--   b <- getLine
--   c <- getLine
--   print [a, b, c]

-- main = do
--   rs <- sequence [getLine, getLine, getLine]
--   print rs

import Control.Monad

-- import Data.Char

-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

main = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          --   color <- getLine
          getLine
      )
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors
