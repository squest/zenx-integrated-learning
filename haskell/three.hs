import Data.List

max' (x: []) = x
max' (x:xs) 
  | x > xsmax = x
  | otherwise = xsmax
  where xsmax = max' xs

bikin_DA un b lim
  | un > lim = []
  | otherwise = un : bikin_DA (un+b) b lim

delete1 e [] = []
delete1 e (x:xs)
  | e == x = xs
  | otherwise = x : (delete1 e xs)

deleteAll e [] = []
deleteAll e (x:xs)
  | e == x = deleteAll e xs
  | otherwise = x : deleteAll e xs

fact n
  | n <= 1 = 1
  | otherwise = n * fact (pred n)

fact' n = fhelper n 1
  where fhelper n res
          | n <= 1 = res
          | otherwise = fhelper (pred n) (n * res)

-- fact' 5 = fhelper 5 1
-- fhelper 5 1 = fhelper 4 (5*1)
-- fehlper 4 5 = fhelper 3 (4 * 5)
-- fhelper 3 20 = fhelper 2 (3*20)
-- fhelper 2 60 = fhelper 1 (2*60)
-- fhelper 1 120 = 120

sum' ls = help ls 0
  where help [] res = res
        help (x:xs) res = help xs (x + res)

-- sum' [2,3,4] = help [2,3,4] 0
-- help [2,3,4] 0 = help [3,4] (2+0)
-- help [3,4] 2 = help [4] (3+2)
-- help [4] 5 = help [] (4+5)
-- help [] 9 = 9

-- Utama accumulator
-- (Utama => elemen 1) (elemen 1 => acc)

-- lambda function

square x = x * x
sqr = \x -> x * x

-- higher order function => f input

-- map f ls => [f e1,f e2, f e3..]

-- foldl1 (+) [1,2,3,4]
-- ((1 + 2) + 3) + 4

max2 a b = if a > b then a else b

prime' n
  | n < 10 = elem n [2,3,5,7]
  | even n = False
  | otherwise = factor n == [1,n]
  where factor n = [x|x <- [1..n], mod n x == 0]

primes = filter prime' [1..]

-- map f ls =>

-- iterate succ 1
-- [1]
-- [1] ++ [succ 1] => [1,2]
-- [1,2] ++ [succ 2] => [1,2,3]
-- [1,2,3] ++ [succ 3] => [1,2,3,4]

fibolist i = reverse $ res !! (i - 2)
  where res = iterate fhelper [1,1]
        fhelper (x:y:xs) = x+y : (x:y:xs)









