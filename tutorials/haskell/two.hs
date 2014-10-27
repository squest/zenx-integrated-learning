module Two where
import Data.List

test x = x * x

sqr x = x * x

-- sqr (sqr 10)
-- (sqr 10) * (sqr 10)
-- 10 * 10 * 10 * 10

-- n! = 1 kalo n=0
-- n! = n * (n-1)! <= n > 0

-- 10.9.8.7.6.5.4.3.2.1 

fak 0 = 1
fak 1 = 1
fak 2 = 2
fak n = n * (fak (n - 1))

fact 0 = 1
fact n = n * (fact (n-1))

pangkat4 x = (sqr (sqr x))

abs' x = if x >= 0
         then x
         else (-x)

abs'' x
  | x >= 0 = x
  | otherwise = -x

pkt4' x = (sqr x) * (sqr x)

pkt4 x = a * a
  where a = (sqr x)

prima = [2,3,5,7]

-- Int, Integer => Integral
-- Bool True & False

fako y = sum [x^4| x <- [1..y]]

-- x => quantity

basis10 x
  | x < 10 = [x]
  | otherwise = sisa ++ [mod x 10]
  where sisa = basis10 (div x 10)

        

-- 1234 x 4 => 1,2,3,4
-- 1250 - 16 (* 4)
-- 1000 + 200 + 30 + 4










