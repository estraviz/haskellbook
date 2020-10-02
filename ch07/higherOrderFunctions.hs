module HigherOrderFunctions where

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

fSub = flip' (-)
fDiv = flip' (/)

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x 

