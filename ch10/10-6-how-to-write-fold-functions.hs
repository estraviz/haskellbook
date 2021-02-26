module HowToWriteFoldFunctions where

main = do
  let pab = ["Pizza", "Apple", "Banana"]

  print $ map (take 3) pab
  print ""
  print "Both expressions equal: Piz"
  print $ foldr (\a b -> take 3 a) "" pab
  print $ head $ map (take 3) pab
  print ""
  print "Both expressions equal: Ban"
  print $ foldl (\b a -> take 3 a) "" pab
  print $ last $ map (take 3) pab
  print ""
  print "Both expressions equal: PizAppBan"
  print $ concat $ map (take 3) pab
  let f = (\a b -> take 3 a ++ b)
  print $ foldr f "" pab
  print ""
  print "Both expressions equal: BanAppPiz"
  let rpab = reverse pab
  print $ concat $ map (take 3) rpab
  let f' = (\b a -> take 3 a ++ b)
  print $ foldl f' "" pab
  print ""
  let f a b = take 3 (a :: String) ++ (b :: String)
  print $ foldr f "" pab
