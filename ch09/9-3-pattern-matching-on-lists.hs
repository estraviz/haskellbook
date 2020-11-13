module PatternMatchinOnLists where

-- myHead :: [t] -> t
-- myHead []      = []
-- myHead (x : _) = x

myTail :: [t] -> [t]
myTail []       = []
myTail (_ : xs) = xs

safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (_ : []) = Nothing
safeTail (_ : xs) = Just xs

