import Data.List

-- n! = 1 kalo n = 0
-- n! = n * (n-1)!

sum' [] = 0
sum' xs = (head xs) + sum' (tail xs)

product' [] = 1
product' xs = (head xs) * product' (tail xs)

sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

prod [] = 1
prod (x: []) = x
prod (x:y:xs) = x * y * prod xs

prod' [] = 1
prod' (x:xs)
  | null xs = x
  | otherwise = x * prod' xs

fungsi x = [[x+a, x+b] | b <- [1..100], a <- [2..100]]

                

-- sum' [1,2,3] = 1 + sum' [2,3]
--              = 2 + sum' [3]
--              = 3 + sum' []

