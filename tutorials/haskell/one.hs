import Data.List

test1 x = x * x * x

-- y = f(x) = x^2 + x

-- f(x) => (f x)

cube x = (sqr x) * x

sqr x = x * x

-- sqr 10 = 10 * 10

akar x = x * x* x

our_pi = 3.141332424243

-- data + functions
-- data => domains
-- data types Int, Integer, Char, Float
-- data type Bool

lista = [1,3..30]

mutlak x = if x >= 0 then x else (-x)

-- 0 <= x <= 10 -> x

mut x = if (x >= 0) && (x <= 10) then x else (-x)

mutLaK' x
  | x >= 0 = x
  | otherwise = (-x)

fun1 x
  | (x > 10) = x + 2
  | ((0 <= x) && (x <= 10)) = 3 * x
  | otherwise = (-x)

even' p = (0 == (mod p 2))

genap lim = [x | x <- [1..lim], even' x]

lista2 lim = [x | x <- [1..lim],
              (0 == (mod x 3)) || (0 == (mod x 7))]

-- { x | x E bulat, x itu genap }
-- DRY -> dont repeat yourself

is_div a m = (0 == (mod a m))

sol1a lim = sum [x | x <- [1..lim],
                 (is_div x 3) || (is_div x 5)]

-- latihan 1. Bikin factorial
-- latihan 2. jumlah sebuah deret aritmatika
-- latihan2 a b n
-- latihan 3. rata2x deret di lat2

fak1 n = product [1..n]

fak 0 = 1
fak n = n * (fak $ pred n)

jum_der a b n = sum $ take n [a,(a+b)..]

rata2x_der a b n = jumDer `div` n
  where jumDer = jum_der a b n





















