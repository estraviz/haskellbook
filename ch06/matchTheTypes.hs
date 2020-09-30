module MatchTheTypes where

import Data.List

-- Ex.1
--    1.a) ok
i :: Num a => a
-- i = 1
--    1.b) ko
-- i :: a
i = 1

-- Ex.2
--    2.a) ok 
f :: Float
--    2.b) ko
-- f :: Num a => a
f = 1.0

-- Ex.3
--    3.a) ok
-- f' :: Float
--    3.b) ok
f' :: Fractional a => a
f' = 1.0

-- Ex.4 
--    4.a) ok
-- f'' :: Float
--    4.b) ok 
f'' :: RealFrac a => a
f'' = 1.0

-- Ex.5
--    5.a) ok
-- freud :: a -> a 
--    5.b) ok
freud :: Ord a => a -> a
freud x = x

-- Ex.6
--    6.a) ok
-- freud' :: a -> a
--    6.b) ok
freud' :: Int -> Int
freud' x = x

-- Ex.7
--    6.a) ok
myX = 1 :: Int

sigmund :: Int -> Int
--    6.b) ko
-- sigmund :: a -> a
sigmund x = myX

-- Ex.8
myX' = 1 :: Int
--   8.a) ok
sigmund' :: Int -> Int
--   8.b) ko 
-- sigmund' :: Num a => a -> a
sigmund' x = myX'

-- Ex.9
--    9.a) ok
-- jung :: Ord a => [a] -> a
--    9.b) ok
jung :: [Int] -> Int
jung xs = head (sort xs)

-- Ex.10
--    10.a) ok
-- young :: [Char] -> Char
--    10.b) ok
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- Ex.11
mySort :: [Char] -> [Char]
mySort = sort
--    11.a) ok
signifier :: [Char] -> Char
--    11.b) ko
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

