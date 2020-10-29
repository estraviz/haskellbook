module ListComprehensions where

squares :: (Num a, Ord a, Enum a) => a -> [a]
squares n 
  | n >= 0    = [x^2 | x <- [1..n]]
  | otherwise = []

evenSquares :: Integral a => a -> [a]
evenSquares n
  | n >= 0    = [x^2 | x <- [1..n], rem x 2 == 0]
  | otherwise = []

squaresAndCubesLessThan200 :: Integral a => a -> [a]
squaresAndCubesLessThan200 n
  | n >= 0    = [x ^ y | x <- [1..n], y <- [2, 3], x ^ y < 200]
  | otherwise = [] 

mySqr = [x^2 | x <- [1..10]]

myAcronym = [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]

myString xs = [x | x <- xs, elem x "aeiou"]


-- Exercise: Square cube
mySqr' = [x^2 | x <- [1..5]]
myCube = [y^3| y <- [1..5]]
-- ex 1
myTuples = [(x, y) | x <- mySqr', y <- myCube]
-- ex 2
myTuples' = [(x, y) | x <- mySqr', y <- myCube, x < 50, y < 50]
-- ex 3
myTuplesLength = length myTuples'

main = do
  print mySqr
  print myAcronym
  print $ myString "hola"
  print myTuples
  print myTuples'
  print myTuplesLength

