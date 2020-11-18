module FoldRightAndLeft where


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (foldr f z xs)

myFoldr' :: (a -> b -> b) -> b -> [a] -> b
myFoldr' f z xs = case xs of []     -> z
                             (x:xs) -> f x (foldr f z xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

const' :: a -> b -> a
const' x _ = x

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = foldl f (f acc x) xs


main :: IO ()
main = do
  print "FOLDR"
  print "-----"
  print $ foldr (+) 0 [1, 2, 3]

  print "foldr associativity works like this:"
  let xs = map show [1..5]
  let y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs
  print $ y

  print $ foldr (+) 0 [1..5]

  print $ myAny even [1..]

  let u = undefined
  let xs = take 4 ([1, 2, 3, 4] ++ u)
  print $ foldr (+) 0 xs

  print $ length [1, 2, 3, 4, undefined]

  let xs = [1, 2, 3, 4] ++ undefined
  print $ length (take 4 xs)

  let xs = [1, 2] ++ undefined
  print $ length $ take 2 $ take 4 xs

  let its9001 = (\_ _ -> 9001)
  let xs = [1..5]
  print $ foldr its9001 0 xs
  let xs = [1, undefined]
  print $ foldr its9001 0 xs
  let xs = [undefined, undefined]
  print $ foldr its9001 0 xs

  print $ foldr const' 0 [1..5]
  print $ foldr const' 0 ([1, 2] ++ undefined)

  print "foldr..."
  print $ foldr (+) 0 [1..5]
  print "scanr..."
  print $ scanr (+) 0 [1..5]
  print ""

  print "FOLDL"
  print "-----"
  print "foldl associativity works like this:"
  let conc = concat
  let f x y = conc ["(", x, "+", y, ")"]
  print $ foldl f "0" (map show [1..5])
  print "foldr..."
  print $ foldl (+) 0 [1..5]
  print "scanl..."
  print $ scanl (+) 0 [1..5]

  print $ last (scanl (+) 0 [1..5])
  print $ head (scanr (+) 0 [1..5])
  print ""
  print "Should equal: 1"
  print $ foldr (^) 2 [1..3]
  print "Should equal: 64"
  print $ foldl (^) 2 [1..3]
  print "Should equal [1, 2, 3]"
  print $ foldr (:) [] [1..3]
  print "Should equal [3, 2, 1]"
  print $ foldl (flip (:)) [] [1..3]
