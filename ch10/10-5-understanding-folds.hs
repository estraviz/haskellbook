module UnderstandingFolds where

main = do
  print "-- 1."
  print $ foldr (*) 1 [1..5]
  print "-- 1.a. -> error"
  --print $ flip (*) 1 [1..5]
  print "-- 1.b."
  print $ foldl (flip (*)) 1 [1..5]
  print "-- 1.c."
  print $ foldl (*) 1 [1..5]
  print "-- 2."
  print $ foldl (flip (*)) 1 [1..3]
  print "-- 5."
  print "-- 5.a."
  print $ foldr (++) [] ["woot", "WOOT", "woot"]
  print "-- 5.b."
  print $ foldr max ' ' "fear is the little death"
  print "-- 5.c."
  print $ foldr (&&) True [False, True]
  print "-- 5.d."
  print $ foldr (||) False [False, True]
  print "-- 5.e."
  print $ foldl (++) "" (map show [1..5])
  print "-- 5.f."
  print $ foldr const 0 [1..5]
  print "-- 5.g."
  print $ foldr const ' ' "tacos"
  print "-- 5.h."
  print $ foldl (flip const) ' ' "burritos"
  print "-- 5.i."
  print $ foldl (flip const) 0 [1..5]
