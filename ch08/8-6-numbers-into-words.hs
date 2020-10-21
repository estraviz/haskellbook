module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of 0 -> "zero"
                          1 -> "one"
                          2 -> "two"
                          3 -> "three"
                          4 -> "four"
                          5 -> "five"
                          6 -> "six"
                          7 -> "seven"
                          8 -> "eight"
                          9 -> "nine"
                          _ -> error "Not a valid number"

digits :: Int -> [Int]
digits n = go n []
  where go n' acc
         | n' < 10 = [n'] ++ acc
         | otherwise = go (div n' 10) ([mod n' 10] ++ acc)

wordNumber :: Int -> String
wordNumber n = 
  concat . (intersperse "-") . (map digitToWord) . digits $ n

main = do
  wordNumber $ 0123456789 -- equal to: "one-two-three-four-five-six-seven-eight-nine"

