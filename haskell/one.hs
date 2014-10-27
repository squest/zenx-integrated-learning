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

sol2 = sum [abs 50+x| x <- [1,3..99]] + sum [abs (50-x)| x <- [2,4..100]]

numcol x
  | x < 10 = [x]
  | otherwise = (numcol (div x 10)) ++ [rem x 10]


numcol6 x
  | x < 6 = [x]
  | otherwise = (numcol6 (div x 6)) ++ [rem x 6]

jumFakDigit n = sum $ map faktorial (numcol n)

sol4 lim = sum $ map jumFakDigit [1..lim]




