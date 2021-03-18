import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    hPutStr,
    openFile,
    openTempFile,
  )

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove), ("bump", bump)]

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

bump2 :: [String] -> IO ()
bump2 [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      selectedTask = todoTasks !! number
      newTodoTasks = selectedTask : delete selectedTask todoTasks
  hPutStr tempHandle $ unlines newTodoTasks
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- T.readFile fileName
  let number = read numberString
      todoTasks = T.lines contents
      selectedTask = todoTasks !! number
      newTodoTasks = T.unlines (selectedTask : delete selectedTask todoTasks)
  T.writeFile fileName newTodoTasks

errorExit :: String -> IO Bool
errorExit command = do
  let commands = map fst dispatch
  if command `notElem` commands then doFailure else pure False
  where
    doFailure = do
      putStrLn $ "Invalid input command: " ++ command
      exitFailure

main = do
  (command : args) <- getArgs
  let (Just action) = lookup command dispatch
  inputErr <- errorExit command
  if not inputErr then action args else pure ()
