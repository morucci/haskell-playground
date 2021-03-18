import Data.List
import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The programe name is:"
  putStrLn progName
