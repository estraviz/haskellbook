module Chapter10.Exercises where

-- ### Warm-up and review ###
-- 1.
stops  = "pbtdkg"
vowels = "aeiou"
---- 1.a
tupleThree :: [a] -> [b] -> [c] -> [(a, b, c)]
tupleThree a b c = [(x, y, z) | x <- a, y <- b, z <- c]

---- 1.b
tupleThreeStartsWith :: Eq a => a -> [a] -> [b] -> [c] -> [(a, b, c)]
tupleThreeStartsWith a0 a b c = [(x, y, z) | x <- a, x == a0, y <- b, z <- c]

---- 1.c
nouns = ["Patxi", "Imanol", "Javi"]
verbs = ["fears", "enjoys", "loves"]
langs = ["Haskell", "Scala", "Python"]

-- 2.
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- ### Rewriting functions using folds ###
-- Point free versions look like this:
-- myFunc = foldr f z

-- 'and' function implementations (examples for myAnd):
-- Direct recursion, not using &&
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- Direct recursion, using &&
myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs) = x && myAnd' xs

-- Fold, not point-free
myAnd'' :: [Bool] -> Bool
myAnd'' = foldr (\x xs -> if x == False then False else xs) True

-- Fold, both myAnd and the folding function are point-free
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

-- 1. myOr
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = if x == True then True else myOr xs

myOr' :: [Bool] -> Bool
myOr' []     = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (\x xs -> if x == True then True else xs) False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

-- 2. myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = if (f x) then True else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f []    = False
myAny' f(x:xs) = f x || myAny' f xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f = foldr (\x xs -> f x || xs) False

myAny''' :: (a -> Bool) -> [a] -> Bool
myAny''' f = myOr''' . map f

-- 3. myElem (the exercises is asking for the implementations myElem'' and myElem''')
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = if x == y then True else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (\x -> x == a) xs

-- version that uses folding
myElem'' :: Eq a => a -> [a] -> Bool
myElem'' a = foldr (\x xs -> x == a || xs) False

-- version that uses any
myElem''' :: Eq a => a -> [a] -> Bool
myElem''' = any . (==)

-- 4. myReverse
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 5. myMap using folding
-- the following one, recursive, w/o folding
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- the next two, by using foldr
myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x xs -> f x : xs) []

myMap'' :: (a -> b) -> [a] -> [b]
myMap'' f = foldr ((:) . f) []

-- 6. myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x : xs else xs) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x -> if f x then (x:) else id) []

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [x]    = x
squish (x:xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = concat

squish'' :: [[a]] -> [a]
squish'' = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish'' . map f

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr (\x xs -> f x ++ xs) []

squishMap'' :: (a -> [b]) -> [a] -> [b]
squishMap'' f = foldr ((++) . f) []

-- 9. squishAgain flattens a list of lists into a list by reusing the squishMap function
squishAgain :: [[a]] -> [a]
squishAgain = squishMap'' id

-- 10. myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x]  = x
myMaximumBy f (x:xs)
  | f x y     == GT = x
  | otherwise       = y
  where y = myMaximumBy f xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:xs) = foldl go x xs
  where go x max = case (f x max) of GT -> x
                                     EQ -> max
                                     LT -> max

myMaximumBy'' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy'' f (x:xs) = foldl (\a b -> if f a b == GT then a else b) x xs

-- 11. myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x]    = x
myMinimumBy f (x:xs)
  | f x y == LT = x
  | otherwise   = y
  where y = myMinimumBy f xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f (x:xs) = foldl go x xs
  where go x min = case (f x min) of GT -> min
                                     EQ -> min
                                     LT -> x

myMinimumBy'' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy'' f (x:xs) = foldl (\a b -> if f a b == LT then a else b) x xs


-- MAIN (results, for testing the implementations above)
main = do
  print "## Warm up and review ##"
  print "-- 1."
  print "---- 1.a"
  print $ tupleThree stops vowels stops
  print "---- 1.b"
  print $ tupleThreeStartsWith 'p' stops vowels stops
  print "---- 1.c"
  print $ tupleThree nouns verbs langs
  print $ tupleThreeStartsWith "Javi" nouns verbs langs
  print "-- 2."
  print $ seekritFunc "This is the average words length"
  print $ seekritFunc' "This is the average words length"
  print ""
  print "## Rewriting functions using folds ##"
  print "-- 0. myAnd"
  print $ myAnd    [False, False] == False
  print $ myAnd'   [True, False]  == False
  print $ myAnd''  [False, True]  == False
  print $ myAnd''' [True, True]   == True
  print "-- 1. myOr"
  print $ myOr    [False, False] == False
  print $ myOr'   [True, False]  == True
  print $ myOr''  [True, False]  == True
  print $ myOr''' [True, False]  == True
  print "-- 2. myAny"
  print $ myAny    even [1, 3, 5] == False
  print $ myAny    odd  [1, 3, 5] == True
  print $ myAny'   even [1, 3, 5] == False
  print $ myAny'   odd  [1, 3, 5] == True
  print $ myAny''  even [1, 3, 5] == False
  print $ myAny''  odd  [1, 3, 5] == True
  print $ myAny''' even [1, 3, 5] == False
  print $ myAny''' odd  [1, 3, 5] == True
  print "-- 3. myElem"
  print $ myElem    1 [1..10] == True
  print $ myElem    1 [2..10] == False
  print $ myElem'   1 [1..10] == True
  print $ myElem'   1 [2..10] == False
  print $ myElem''  1 [1..10] == True
  print $ myElem''  1 [2..10] == False
  print $ myElem''' 1 [1..10] == True
  print $ myElem''' 1 [2..10] == False
  print "-- 4. myReverse"
  print $ myReverse  "blah" == "halb"
  print $ myReverse  [1..5] == [5, 4, 3, 2, 1]
  print $ myReverse' "blah" == "halb"
  print $ myReverse' [1..5] == [5, 4, 3, 2, 1]
  print "-- 5. myMap"
  let double x = 2 * x
  print $ myMap   double [1, 2..5] == [2, 4..10]
  print $ myMap'  double [1, 2..5] == [2, 4..10]
  print $ myMap'' double [1, 2..5] == [2, 4..10]
  print "-- 6. myFilter"
  print $ myFilter  even [1..10] == [2, 4..10]
  print $ myFilter  odd  [1..10] == [1, 3..9]
  print $ myFilter' even [1..10] == [2, 4..10]
  print $ myFilter' odd  [1..10] == [1, 3..9]
  print "-- 7. squish"
  let aList = [[1,2], [3], [4, 5, 6]]
  print $ squish   aList == [1, 2, 3, 4, 5, 6]
  print $ squish'  aList == [1, 2, 3, 4, 5, 6]
  print $ squish'' aList == [1, 2, 3, 4, 5, 6]
  print "-- 8. squishMap"
  let f x = "WO " ++ [x] ++ " OT "
  print $ squishMap   (\x -> [1, x, 3]) [2] == [1..3]
  print $ squishMap   f "blah"              == "WO b OT WO l OT WO a OT WO h OT "
  print $ squishMap'  (\x -> [1, x, 3]) [2] == [1..3]
  print $ squishMap'  f "blah"              == "WO b OT WO l OT WO a OT WO h OT "
  print $ squishMap'' (\x -> [1, x, 3]) [2] == [1..3]
  print $ squishMap'' f "blah"              == "WO b OT WO l OT WO a OT WO h OT "
  print "-- 9. squishAgain"
  print $ squishAgain aList == [1..6]
  print "-- 10. myMaximumBy"
  print $ myMaximumBy compare [1, 53, 9001, 10]       == 9001
  print $ myMaximumBy compare [-1, -53, -9001, -10]   == -1
  print $ myMaximumBy (\_ _ -> GT) [1..10]            == 1
  print $ myMaximumBy (\_ _ -> LT) [1..10]            == 10
  print $ myMaximumBy compare [1..10]                 == 10
  print ""
  print $ myMaximumBy' compare [1, 53, 9001, 10]      == 9001
  print $ myMaximumBy' compare [-1, -53, -9001, -10]  == -1
  print $ myMaximumBy' (\_ _ -> GT) [1..10]           == 1
  print $ myMaximumBy' (\_ _ -> LT) [1..10]           == 10
  print $ myMaximumBy' compare [1..10]                == 10
  print ""
  print $ myMaximumBy'' compare [1, 53, 9001, 10]     == 9001
  print $ myMaximumBy'' compare [-1, -53, -9001, -10] == -1
  print $ myMaximumBy'' (\_ _ -> GT) [1..10]          == 1
  print $ myMaximumBy'' (\_ _ -> LT) [1..10]          == 10
  print $ myMaximumBy'' compare [1..10]               == 10
  print "-- 11. myMinimumBy"
  print $ myMinimumBy compare [1, 53, 9001, 10]       == 1
  print $ myMinimumBy compare [-1, -53, -9001, -10]   == -9001
  print $ myMinimumBy (\_ _ -> GT) [1..10]            == 10
  print $ myMinimumBy (\_ _ -> LT) [1..10]            == 1
  print $ myMinimumBy compare [1..10]                 == 1
  print ""
  print $ myMinimumBy' compare [1, 53, 9001, 10]      == 1
  print $ myMinimumBy' compare [-1, -53, -9001, -10]  == -9001
  print $ myMinimumBy' (\_ _ -> GT) [1..10]           == 10
  print $ myMinimumBy' (\_ _ -> LT) [1..10]           == 1
  print $ myMinimumBy' compare [1..10]                == 1
  print ""
  print $ myMinimumBy'' compare [1, 53, 9001, 10]     == 1
  print $ myMinimumBy'' compare [-1, -53, -9001, -10] == -9001
  print $ myMinimumBy'' (\_ _ -> GT) [1..10]          == 10
  print $ myMinimumBy'' (\_ _ -> LT) [1..10]          == 1
  print $ myMinimumBy'' compare [1..10]               == 1
