import Data.List

sqr x = x * x

add2 x = x + 2

sum' xs
  | null xs = 0
  | otherwise = (head xs) + (sum (tail xs))

sum'' [] = 0
sum'' (x:xs) = x + (sum'' xs)

faktorial n
  | n == 0 = 1
  | otherwise = n * (faktorial (pred n))

fakt 0 = 1
fakt n = n * (fakt (pred n))


sum''' [] = 0
sum''' (x:xs) = x + sum''' xs

prod' [] = 1
prod' (x:xs) = x * prod' xs
