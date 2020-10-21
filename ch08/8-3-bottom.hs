module Bottom where

f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
           ++ "Non-exhaustive "
           ++ "patterns in function f"

f' :: Bool -> Maybe Int
f' False = Just 0
f' _     = Nothing

