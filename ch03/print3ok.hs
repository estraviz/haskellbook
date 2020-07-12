-- print3ok.hs
module Print3OK where

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond

greeting :: String
greeting = "Yarrrrr"
