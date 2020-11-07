module StandardFunctions where

import Data.Char

-- 0. myAnd 

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False
               then False
               else myAnd xs

-- direct recurson, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs) = x && myAnd' xs

-- 1. myOr
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = if x
              then True
              else myOr xs

myOr' :: [Bool] -> Bool
myOr' []     = False
myOr' (x:xs) = x || myOr' xs

-- 2. myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = if (f x)
                 then True
                 else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

-- 3. myElem
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = if x == y
                  then True
                  else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = any (\y -> x == y) ys   

-- 4. myReverse 
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5. squish
squish :: [[a]] -> [a]
squish [x]    = x
squish (x:xs) = x ++ squish xs   

squish' :: [[a]] -> [a]
squish' = concat

-- 6. squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f 

-- 7. squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8. myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
  where go _ max []     = max
        go f max (x:xs) = case f x max of GT -> go f x xs
                                          _  -> go f max xs

-- 9. myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x xs
  where go _ min []     = min
        go f min (x:xs) = case f x min of LT -> go f x xs
                                          _  -> go f min xs 

-- 10. myMaximum, myMinimum
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

-- main
main :: IO ()
main = do
  print "-- 0. myAnd --"
  print "Should return: True"
  print $ myAnd  [True, True]
  print "Should return: False"
  print $ myAnd' [True, False]
  print ""
  print "-- 1. myAnd --"
  print "Should return: True" 
  print $ myOr'  [True, True]
  print "Should return: True"
  print $ myOr'  [True, False]
  print "Should return: True"
  print $ myOr'  [False, True]
  print "Should return: False"
  print $ myOr'  [False, False]
  print ""
  print "-- 2. myAny --"
  print "Should return: False"
  print $ myAny  even [1, 3, 5]
  print "Should return: True"
  print $ myAny  odd  [1, 3, 5]
  print "Should return: False"
  print $ myAny' even [1, 3, 5]
  print "Should return: True"
  print $ myAny' odd  [1, 3, 5]
  print ""
  print "-- 3. myElem --"
  print "Should return: True"
  print $ myElem' 1 [1..10]
  print "Should return: False"
  print $ myElem' 1 [2..10]
  print "Should return: True"
  print $ myElem' 1 [1..10]
  print "Should return: False"
  print $ myElem' 1 [2..10]
  print ""
  print "-- 4. myReverse --"
  print "Should return: halb"
  print $ myReverse "blah"
  print "Should return: [5, 4, 3, 2, 1]"
  print $ myReverse [1..5]
  print ""
  print "-- 5. squish --"
  print "Should return: [1, 2, 3, 4, 5, 6]"
  print $ squish [[1, 2, 3], [4, 5], [6]]
  print "Should return: [6, 5, 4, 3, 2, 1]"
  print $ squish [[6, 5], [4], [3, 2, 1]]
  print "Should return: abcdef"
  print $ squish [['a', 'b'], ['c'], ['d', 'e', 'f']]
  print ""
  print "-- 6. squishMap --"
  print "Should return: [1, 2, 3]"
  print $ squishMap (\x -> [1, x, 3]) [2]
  print "Should return: WO 1 HOO WO 2 HOO WO 3 HOO "
  print $ squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123"
  print ""
  print "-- 7. squishAgain --"
  print "Should return: [1, 2, 3, 4, 5, 6]"
  print $ squishAgain [[1], [2, 3], [4, 5, 6]]
  print ""
  print "-- 8. myMaximumBy --"
  print "Should return: 9001"
  print $ myMaximumBy compare [1, 53, 9001, 10]
  print "Should return: -1"
  print $ myMaximumBy compare [-1, -53, -9001, -10]
  print ""
  print "-- 9. myMinimumBy --"
  print "Should return: 1"
  print $ myMinimumBy compare [1, 53, 9001, 10]
  print "Should return: -9001"
  print $ myMinimumBy compare [-1, -53, -9001, -10]
  print ""
  print "-- 10. myMaximum, myMinimum --"
  print "Should return: 9001"
  print $ myMaximum [1, 53, 9001, 10]
  print "Should return: -1"
  print $ myMaximum [-1, -53, -9001, -10]
  print "Should return: 1"
  print $ myMinimum [1, 53, 9001, 10]
  print "Should return: -9001"
  print $ myMinimum [-1, -53, -9001, -10]

