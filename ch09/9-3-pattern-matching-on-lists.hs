module PatternMatchinOnLists where

-- myHead :: [t] -> t
-- myHead []      = []
-- myHead (x : _) = x

myTail :: [t] -> [t]
myTail []       = []
myTail (_ : xs) = xs

-- safeHead :: [a] -> Maybe a
-- safeHead []       = Nothing
-- safeHead ([] : _) = Nothing
-- safeHead (xs : _) = Just xs

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (_ : []) = Nothing
safeTail (_ : xs) = Just xs

