module Ex51 where

rvrs        = (drop 9 txt) ++ (take 4 (drop 5 txt)) ++ (take 5 txt)
  where txt = "Curry is awesome"
