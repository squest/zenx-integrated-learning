import Data.List

sample = [1..20]

-- list comprehension!!!

-- { x | x E A , x genap }

lista = [ x | x <- [1..10], odd x]

listb = [x*x | x <- [1..20], even x]

cube i = i*i*i

listc = [cube x | x <- [-10..10], x > 0]

kel35 i = (mod i 3 == 0) || (mod i 5 == 0)

listd = [x | x <- [1..100], kel35 x]

kels35 i j = [x| x <- [i..j], kel35 x]

kels37 lim = [x^3 | x <- [1..lim],
              (mod x 3 == 0) || (mod x 7 == 0)]

kurangdari10 i = if i < 10
                 then True
                 else False

abs' x
  | x >= 0 = x
  | otherwise = -x

kecap a b c i
  | i == 1 = ((-b) - (sqrt disk)) / (2 * a)
  | i == 2 = ((-b) + (sqrt disk)) / (2*a)
  where disk = (b^2) - (4*a*c)

factors n = [x | x <- [1..n], mod n x == 0]

prime p = (factors p) == [1,p]

kels35' i j= [x | x <- [i..j],
              (mod x 3 == 0) || (mod x 5 == 0)]

euler1 = sum $ kels35' 1 999

f1 i j = [x | x <- [i..j],
          (abs' (x+5)) > 5,  (abs' (x+5)) < 20]








