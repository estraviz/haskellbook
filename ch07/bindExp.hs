module BindExp where

bindExp :: Integer -> String
bindExp x = 
  let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

-- shadowing
bindExp' :: Integer -> String
bindExp' x = 
  let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

