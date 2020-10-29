module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool True True   = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft
--eftOrd a b
--  | compare a b == GT = []
--  | compare a b == EQ = [a]
--  | otherwise         = [a, b]

eftInt :: Int -> Int -> [Int]
eftInt = eft
--eftInt a b
--  | a > b  = []
--  | a == b = [a]
--  | a < b  = a : eftInt (succ a) b    

eftChar :: Char -> Char -> [Char]
eftChar = eft
--eftChar a b
--  | a > b     = []
--  | a == b    = [a]
--  | otherwise = [a] ++ eftChar (succ a) b

--eft
eft a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eft (succ a) b 

