module Spines where

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

