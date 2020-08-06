module MyAbs where

myAbs :: Integer -> Integer
myAbs absNum = 
  if absNum >= 0
    then absNum
  else
    (-1) * absNum 

