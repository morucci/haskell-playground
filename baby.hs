doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

zipDouble :: Num a => [a] -> [a] -> [a]
zipDouble x' y' = [x * y | x <- x', y <- y']

removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

factoriel :: Integer -> Integer
factoriel n = product [1 .. n]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

myfunc :: Integer -> Integer
myfunc z = z + 3 + 1 + succ 10

first :: (a, b, c) -> a
first (x, _, _) = x

lrest :: [a] -> [a]
lrest (_ : xs) = xs
lrest _ = []

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmi :: RealFloat a => a -> a -> a
bmi w h = w / h ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi' <= skinny = "You're underweight, you emo, you!"
  | bmi' <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi' <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi' = bmi weight height
    skinny = 18.5
    normal = 25.0
    fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
