module AddModule where

add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Num a, Ord a)  => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x

addInts :: Int -> Int -> Int
addInts x y = x + y

addWeirdInts :: Int -> Int -> Int
addWeirdInts x y =
  if x > 1
  then x + y
  else x

check' :: Int -> Int -> Bool
check' a a' = a == a'

