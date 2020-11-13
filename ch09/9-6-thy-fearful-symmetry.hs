module PoemLines where

-- 1.
myWords :: String -> [String]
myWords [] = []
myWords x  = [xs] ++ myWords x' 
  where xs = takeWhile (/= ' ') x
        x' = dropWhile (== ' ') . dropWhile (/= ' ') $ x

ex1 :: IO ()
ex1 = print $ myWords "sheryl wants fun"

-- 2.
firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

poem :: IO ()
poem = putStrLn sentences

myLines :: String -> [String]
myLines [] = []
myLines x  = [xs] ++ myLines x'
  where xs = takeWhile (/= '\n') x
        x' = dropWhile (== '\n') . dropWhile (/= '\n') $ x 

shouldEqual = 
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

ex2 :: IO ()
ex2 = 
  print $
  "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)

-- 3.
breakOnChar :: Char -> String -> [String]
breakOnChar _ [] = []
breakOnChar c x  = [xs] ++ breakOnChar c x'
  where       xs = takeWhile (/= c) x
              x' = dropWhile (== c) . dropWhile (/= c) $ x 

-- 3.1
myWords' :: String -> [String]
myWords' = breakOnChar ' '

ex31 :: IO ()
ex31 = print $ myWords' "sheryl wants fun"

-- 3.2 
myLines' :: String -> [String]
myLines' = breakOnChar '\n'

ex32 :: IO ()
ex32 =
  print $ 
  "Are they (still) equal? "
  ++ show (myLines' sentences
           == shouldEqual)

