module Zipping where


-- 1.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' x y  = (head x, head y) : zip' (tail x) (tail y) 

zip'' :: [a] -> [b] -> [(a, b)]
zip'' [] _          = []
zip'' _ []          = []
zip'' (x:xs) (y:ys) = (x, y) : zip'' xs ys

-- 2.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _          = []
zipWith' f _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3.
zipBetter :: [a] -> [b] -> [(a, b)]
zipBetter [] _ = []
zipBetter _ [] = []
zipBetter x y  = zipWith (,) x y  

zipBetter' :: [a] -> [b] -> [(a, b)]
zipBetter' = zipWith (,) 

-- main
main = do
  print $ zip   [1, 2, 3] [4, 5, 6]
  print $ zip'  [1, 2, 3] [4, 5, 6]
  print $ zip'' [1, 2, 3] [4, 5, 6]

  print $ zipWith  (+) [1, 2, 3] [10, 11, 12]
  print $ zipWith' (+) [1, 2, 3] [10, 11, 12]

  print $ zipWith  (*) [1, 2, 3] [10, 11, 12]
  print $ zipWith' (*) [1, 2, 3] [10, 11, 12]

  print $ zipBetter  [1, 2, 3] [4, 5, 6]
  print $ zipBetter' [1, 2, 3] [4, 5, 6]

