module Filtering where

-- 1.
ex1  = filter (\x -> rem x 3 == 0)
ex1' = [x | x <- [1..30], rem x 3 == 0]

-- 2.
ex1Length = length . ex1 

-- 3.
myFilter = filter (\x ->  notElem x ["the", "a", "an"]) . words

main = do
  print $ ex1 [1..30]
  print $ ex1'
  print $ ex1Length [1..30]
  print $ myFilter "the brown dog was a goof"

