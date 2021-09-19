import Control.Monad.Writer.Lazy
import Data.Functor.Identity
  
logNumber :: Int -> Writer [String] Int  
logNumber x = WriterT (Identity (x, ["Got number: " ++ show x]))
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) 

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)
