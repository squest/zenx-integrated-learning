import Data.List

-- functional programming

-- x -> y
-- y = f(x) = x^5 + x^3 + 10

-- data trans. (data & fn)
-- data : types
-- fn : domain & codomain
-- fn : input & output

-- Predicates => pengujian (True/False)

-- Identity!!!!!!!!! -> fn & value

my_pi = 6.28
bilangan_e = 123.24234344

-- f(x) = x^2
-- f y = y^2
-- f z = x^2 + y^2

kuadrat_njing x = x * x

-- (19*3) * (19*3) =>

square n = n * n

-- f (x,y) => program linear

add a b = a + b

-- Tipe 1: => Num -> Num => 10 fns
-- Tipe 2: => predicate => Num =>  Bool 
-- Ngecek even' n, odd' n, kel3kah n,
-- kel3dan5kah n, kel3atau5kah  n,
-- pos_kah n, neg_kah n

kecap a b c = ((-b) + (sqrt disk)) / duaA
  where disk = (b^2) - (4*a*c)
        duaA = 2*a

tigatambah5 = 3+5

even' n = (mod n 2) == 0

faktor a m = (mod a m) == 0

pos_kah n = n > 0

kel3atau5 n = (faktor n 3) || (faktor n 5)

kel3or5 n = (mod n 3 == 0) || (mod n 5 == 0)

-- Primitive Num, Char, Bool
-- List & tuples => List

lista = [1,2,3,4,5,2,34,243,423,243]

second xs = head $ tail xs

snd_last xs = last $ init xs
snd_last' xs = head $ tail $ reverse xs
snd_last'' xs = second $ reverse xs

-- Exercises:

listb = [1..20]
third xs = xs !! 2
third' xs = head $ tail $ tail xs

last_third xs = last $ init $ init xs
last_third' xs = xs !! ((length xs) - 3)
l_t xs = head $ tail $ tail $ reverse xs

stglist xs = take stg xs
  where stg = div (length xs) 2

tail_stg xs = drop stg xs
  where stg = div (length xs) 2

bagi2tempel xs = (stglist xs) ++ (reverse $ tail_stg xs)

ntake i n xs = take n buang
  where buang = drop (pred i) xs

evens = [2,4..]

even_take n = take n evens



















