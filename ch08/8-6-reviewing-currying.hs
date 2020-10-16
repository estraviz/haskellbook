module ReviewingCurrying where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

main = do
-- 1.
  print $ appedCatty "woohoo!"
-- 2.
  print $ frappe "1"
-- 3.
  print $ frappe (appedCatty "2")
-- 4.
  print $ appedCatty (frappe "blue")
-- 5.
  print $ cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- 6.
  print $ cattyConny (flippy "Pugs" "are") "awesome" 

