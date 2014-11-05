import Data.List

-- Basic recursion

fak 0 = 1
fak n = n * fak (n-1)

-- pure recursion => tail recursive functions.

sum' [] = 0
sum' ls = (head ls) + (sum' $ tail ls)

-- terima adalah bil di basis 10 jadi basis 7

basis7 n
  | n < 7 = [n]
  | otherwise = (basis7 (div n 7)) ++ [mod n 7]

last'' ls
  | length ls == 1 = head ls
  | otherwise = last'' $ tail ls

-- Pattern matching

head' (x:xs) = x

second' (_:y:_) = y

last' (x:xs)
  | null xs = x
  | otherwise = last' xs

my_last (x: []) = x
my_last (x:xs) = my_last xs

-- head, tail, init, last, product, sum,...
-- ++ : 

prod [] = 1
prod (x:xs) = x * (prod xs)

rev [] = []
rev (x:xs) = (rev xs) ++ [x]

-- [1,2,3,4,5]
-- rev [2,3,4,5] ++ [1]

kali2elm (x:y: []) = [x*y]
kali2elm (x:y:xs) = (x*y) : (kali2elm (y:xs))


kali2elm' (x:y: []) = [x*y]
kali2elm' (x:y:xs) = (x*y) : (kali2elm' xs)

take' 1 (x:xs) = [x]
take' n (x:xs) = x : (take' (n-1) xs)

-- take' 3 [1,2,3,4,5]
-- 1 : (take' 2 [2,3,4,5])
-- 1 : 2 : (take' 1 [3,4,5])
-- 1 : 2 : [3]

tailTake n ls = helperTake n ls []
  where helperTake n (x:xs) res
          | n == 1 = res ++ [x]
          | otherwise = helperTake (pred n) xs hasil
          where hasil = res ++ [x]






