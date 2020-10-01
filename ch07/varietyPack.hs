module VarietyPack where

-- 1.
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)

-- 2.
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (x1, x2, x3) (y1, y2, y3) = ((x1, y1), (x3, y3))

