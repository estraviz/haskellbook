-- Do not write type classes like this! For learning purposes only :)
module NumberishModule where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

-- pretend newtype is data for now
newtype Age = 
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer 
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

-- Function that uses this type class and the two types and instances:
sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA      = toNumber a
        integerOfAPrime = toNumber a'
        summed =
          integerOfA + integerOfAPrime

-- Do NOT use type classes to define default values. Again, for learning purposes only ;)
class Numberish' a where
  fromNumber'     :: Integer -> a
  toNumber'       :: a -> Integer
  defaultNumber  :: a 

instance Numberish' Age where
  fromNumber'  n = Age n
  toNumber' (Age n) = n
  defaultNumber = Age 65

instance Numberish' Year where
  fromNumber' n = Year n
  toNumber' (Year n) = n
  defaultNumber = Year 1988

