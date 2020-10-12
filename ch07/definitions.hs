module DefinitionsCh07 where

-- 1. Binding
blah :: Int
blah = 10

-- 2. Anonymous functions
id = \x -> x

-- 3. Currying
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' = curry' add

f a b = a + b
f'= \a -> (\b -> a + b)

-- 4. Pattern matching
data Blah = Blah

blahFunc :: Blah -> Bool
blahFunc Blah = True

data Identity a =
  Identity a
  deriving (Eq, Show)

unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x

ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True

ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True

data Product a b =
  Product a b
  deriving (Eq, Show)

productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

data SumOfThree a b c =
  FirstPossible a | SecondPossible b | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _)  = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _)  = 2

sumToInt' :: SumOfThree a b c -> Integer
sumToInt' (FirstPossible _) = 0
sumToInt' _                 = 1

-- 5. Bottom
g x = g x  -- applied to values will recurse indefinitely

-- dontDoThis :: Bool -> Bool
-- dontDoThis True = 1

definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True = 1
definitelyDontDoThis False = error "ooops"

-- 6. Higher-order functions (HOF)

-- 7. Composition
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

comp :: (b -> c) -> ((a -> b) -> (a -> c))
comp f g x = f (g x)

-- 8. Point-free

-- 8.1 Not point-free
blahh x = x
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)

-- 8.2 Point-free versions
-- blahh' = id
-- addAndDrop' = const . (1 +)
reverseMkTuple' = flip (,)
reverseTuple' = uncurry (flip (,))
 
