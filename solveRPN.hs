import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFuction [] . words
  where
    foldingFuction (x : y : ys) "*" = (x * y) : ys
    foldingFuction (x : y : ys) "+" = (x + y) : ys
    foldingFuction (x : y : ys) "-" = (y - x) : ys
    foldingFuction (x : y : ys) "/" = (y / x) : ys
    foldingFuction (x : y : ys) "^" = (y ** x) : ys
    foldingFuction (x : xs) "ln" = log x : xs
    foldingFuction xs "sum" = [sum xs]
    foldingFuction xs numberString = read numberString : xs
