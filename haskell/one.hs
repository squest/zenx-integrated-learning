module One where

import Data.List

sqr x = x * x

add2 x = x + 2

expt a m 
  | m == 0 = 1
  | otherwise = a * (expt a (pred m))
