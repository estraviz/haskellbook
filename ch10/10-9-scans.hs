module Scans where


scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q ls = q : (case ls of []   -> []
                                x:xs -> scanl' f (f q x) xs)

fibs :: [Integer]
fibs = 1 : scanl' (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

-- Exercises --
-- 1. First 20 Fibonacci numbers
fibsNth :: Int -> [Integer]
fibsNth n = take n fibs

-- 2. Fibonacci numbers < 100
fibsLT100 :: [Integer]
fibsLT100 = takeWhile (< 100) fibs

-- 3. Factorial with scanl
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fact :: (Num a, Enum a) => Int -> a
fact n = scanl' (*) 1 [1..] !! n

main = do
  print $ scanr  (+) 0 [1..3]
  print $ scanl  (+) 0 [1..3]
  print $ scanl  (+) 1 [1..3]
  print $ scanl' (+) 1 [1..3] == [1, 2, 4, 7]
  print $ fibsN 0
  print $ fibsN 2
  print $ fibsN 6
  print $ fibsNth 20
  print $ fibsLT100
  print $ factorial 5 == fact 5
  print $ fact 5
