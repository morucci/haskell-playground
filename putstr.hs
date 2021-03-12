putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do
  putChar x
  Main.putStr xs

putStrLn :: String -> IO ()
putStrLn str =
  do
    Main.putStr str
    putChar '\n'

main = Main.putStrLn "Fabien"
