module Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar shift = map $ \x -> chr $ ord 'a' + mod ((ord x - ord 'a') + shift) (ord 'z' - ord 'a' + 1)

unCaesar :: Int -> [Char] -> [Char]
unCaesar shift = map $ \x -> chr $ ord 'a' + mod ((ord x - ord 'a') - shift ) (ord 'z' - ord 'a' + 1)

unCaesar' :: Int -> [Char] -> [Char]
unCaesar' shift = caesar (-shift)

main :: IO ()
main = do
  print $ caesar shift message
  print $ unCaesar shift message'
  print $ caesar shift $ unCaesar' shift $ message'
  print $ unCaesar' shift $ caesar shift $ message
  where message  = "egoistheenemy"
        message' = "hjrlvwkhhqhpb"
        shift    = 3
 
