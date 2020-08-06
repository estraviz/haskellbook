-- typeKwonDo.hs
module TypeKwonDo where

-- ex. 0
data Woot
data Blah

f0 :: Woot -> Blah
f0 = undefined

g0 :: (Blah, Woot) -> (Blah, Blah)
g0 (b, w) = (b, f0 w)

-- ex. 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

-- ex. 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

-- ex. 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y)  = (xz x, yz y)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge f g w = fst (g (f w))

munge' :: (x -> y)
       -> (y -> (w, z))
       -> x
       -> w
munge' f g w = fst $ g $ f w

munge'' :: (x -> y)
        -> (y -> (w, z))
        -> x
        -> w
munge'' f g w = fst . g . f $ w

