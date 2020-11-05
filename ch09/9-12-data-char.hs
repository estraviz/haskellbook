module DataChar where

import Data.Char
import Data.Typeable


-- 1.

-- 2.
getUpper :: [Char] -> [Char]
getUpper = filter isUpper

-- 3.
title :: [Char] -> [Char] 
title []     = []
title (x:xs) = toUpper x : xs 

-- 4.
title' :: [Char] -> [Char]
title' []     = []
title' (x:xs) = toUpper x : title' xs

-- 5.
capitalize :: String -> Char
capitalize "" = error "No string avaliable!"
capitalize xs = toUpper $ head xs

-- 6.
capitalize' :: String -> Char
capitalize' = toUpper . head

main = do
  putStrLn "-- Ex.1 --"
  putStr "isUpper :: " 
  putStrLn $ show $ typeOf isUpper
  putStr "toUpper :: "
  putStrLn $ show $ typeOf toUpper
  putStrLn ""
  putStrLn "-- Ex.2 --"
  putStrLn $ getUpper "HbEfLrLxO"
  putStrLn ""
  putStrLn "-- Ex.3 --"
  putStrLn $ title "julie"
  putStrLn ""
  putStrLn "-- Ex.4 --"
  putStrLn $ title' "julie"
  putStrLn ""
  putStrLn "-- Ex.5 --"
  putStrLn $ show $ capitalize "julie"
  putStrLn ""
  putStrLn "-- Ex.6 --"
  putStrLn $ show $ capitalize' "julie" 

