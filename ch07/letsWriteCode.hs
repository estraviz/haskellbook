module LetsWriteCode where

-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
   where xLast = x `div` 10
         d     = xLast `mod` 10

-- 1.a.
tensDigit' :: Integral a => a -> a
tensDigit' x = d
   where xLast = fst . divMod x $ 10
         d     = snd . divMod xLast $ 10

-- 1.b.
-- a bad option for this:
hunsD :: Integral a => a -> a
hunsD x = d2
   where xLastxLast = fst . divMod x $ 10
         xLast      = fst . divMod xLastxLast $ 10
         d2         = snd . divMod xLast $ 10
-- a better option::
hunsD' :: Integral a => a -> a
hunsD' x = d2
   where xLast = fst . divMod x $ 100
         d2    = snd . divMod xLast $ 10

-- 2.
-- with a case expression
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> x
    False -> y 
-- with a guard
foldBool' :: a -> a -> Bool -> a
foldBool' x y b 
  | b == True  = x
  | b == False = y

-- 3.


