-- givenTypeWriteFunc.hs
module GivenTypeWriteFunc where

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = 
  (a, (yToZ (xToY x)))

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = x

r' :: [a] -> [a]
r' x = x ++ x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = (bToC) (aToB a)

a :: (a -> c) -> a -> a
a aToC x = x

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

