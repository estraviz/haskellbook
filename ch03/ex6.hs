module Reverse where

rvrs :: String -> String
rvrs x = unwords $ reverse $ words x

main :: IO ()
main = do
  print $ rvrs txt
  where txt = "Curry is awesome"
