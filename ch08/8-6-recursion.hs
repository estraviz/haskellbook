module Recursion where

-- 2.
sum1toN :: (Eq a, Num a) => a -> a
sum1toN 0 = 0
sum1toN n = n + sum1toN(n - 1)

-- 3.
prod2num :: Integral a => a -> a -> a
prod2num x y
  | y > 0     = x + prod2num x (y - 1)
  | otherwise = 0

