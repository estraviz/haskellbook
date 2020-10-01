module GrabBag where

-- 1.
mTh_a x y z = x * y * z
mTh_b x y = \z -> x * y * z
mTh_c x = \y -> \z -> x * y * z
mTh_d = \x -> \y -> \z -> x * y * z

-- 3.
addOne x = x + 1
addOne' = \x -> x + 1
-- 3.a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1
addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n + 1)
-- 3.b
addFive x y = (if x > y then y else x) + 5
addFive' = \x -> \y -> (if x > y then y else x) + 5
-- 3.c
mflip f = \x -> \y -> f y x
mflip' f x y = f y x
 
