module HigherOrderFunctions where

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

fSub = flip' (-)
fDiv = flip' (/)

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x 

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

