module FoldingEvaluation where


main = do
  let rcf = foldr (:) []
  let xs = [1, 2, 3] ++ undefined
  print $ take 3 $ rcf xs
  print ""
  print $ foldr const 0 [1..5]
  print $ foldr const 0 ([1] ++ undefined)
  print $ head ([1] ++ undefined)
  print ""
  print $ foldl (flip const) 0 [1..5]
  print ""
  let xs = [1..5]
  let f = const
  let z = 0
  print $ foldr f z xs == foldl (flip f) z (reverse xs)
  print ""
  let xs = [1..5]
  print $ foldr (:) [] xs
  print $ foldl (flip (:)) [] xs
  print $ foldr (:) [] xs == foldl (flip (:)) [] (reverse xs)
  print $ foldr (:) [] xs == (reverse $ foldl (flip (:)) [] xs)
