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
-- pattern matching version
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

g' :: (a -> b) -> (a, c) -> (b, c)
g' f' z = (f' . fst $ z, snd z)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5. point-free version of roundTrip
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main = do
  print (roundTrip 4)
  print (id 4)
  print (roundTrip' 5 :: Int)
  print (roundTrip'' 6 :: Int)

