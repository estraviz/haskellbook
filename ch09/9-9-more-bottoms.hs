module MoreBottoms where

import Data.Bool


isItMystery xs = map (\x -> elem x "aeiou") xs
sqr10 = map (^2) [1..10]
minOfInner = map minimum [[1..10], [10..20], [20..30]]
sumLists = map sum [[1..5], [1..5], [1..5]]

-- ex.6: foldBool in ch07/letsWriteCode.hs (case expression, guard, and pattern matching expressions back then). 
-- The following one negates even numbers:
newFoldBool :: [Integer] -> [Integer]
newFoldBool = map (\x -> bool x (-x) (rem x 2 == 0))

main = do
  print $ isItMystery $ "Hello, World!"
  print $ sqr10
  print $ minOfInner
  print $ sumLists
  print $ newFoldBool [1..10]

