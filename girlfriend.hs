import System.IO

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   showHandleState handle
--   putStr contents
--   hClose handle
--   showHandleState handle
--   where
--     showHandleState :: Handle -> IO ()
--     showHandleState handle = do
--       isclosed <- hIsClosed handle
--       print ("Handle is " ++ if isclosed then "closed" else "open")

main = do
  withFile
    "girlfriend.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )
