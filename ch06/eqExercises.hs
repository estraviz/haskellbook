module EqExercises where

-- Ex.1
data TisAnInteger =
  TisAn Integer deriving Show

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn a') =
     a == a'

-- Ex.2
data TwoIntegers =
  Two Integer Integer deriving Show

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') =
     a == a' && b == b'

-- Ex.3
data StringOrInt =
  TisAnInt Int | TisAString String deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = 
     a == a'
  (==) (TisAString x) (TisAString x') =
     x == x'
  (==) _ _ = False

-- Ex.4
data Pair a =
  Pair a a deriving Show

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') =
     a == a' && b == b'

-- Ex.5
data Tuple a b =
  Tuple a b deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
    a == a' && b == b'

-- Ex.6
data Which a =
  ThisOne a | ThatOne a deriving Show

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') =
     a == a'
  (==) (ThatOne b) (ThatOne b') =
     b == b'
  (==) _ _ = False

-- Ex.7
data EitherOr a b =
  Hello a | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = 
     a == a'
  (==) (Goodbye b) (Goodbye b') =
     b == b'
  (==) _ _ = False

