module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbNumber 9002
  , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]

-- 1. Function that filters for DbDate values and returns a list of the UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate []     = []
filterDbDate (x:xs) = case x of DbDate time -> time : filterDbDate xs
                                _           -> filterDbDate xs

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldr filterDate []
  where filterDate (DbDate time) xs = xs ++ time : []
        filterDate _             xs = xs

-- 2. Function that filters for DBNumber values and returns a list of the Integer values inside them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber []     = []
filterDbNumber (x:xs) = case x of DbNumber number -> number : filterDbNumber xs
                                  _               -> filterDbNumber xs

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = foldr filterNumber []
  where filterNumber (DbNumber number) xs = xs ++ number : []
        filterNumber _                 xs = xs

-- 3. Function that gets the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr getRecent (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where getRecent (DbDate time) recent = if time > recent then time else recent
        getRecent _             recent = recent

-- 4. Function that sums all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

sumDb' :: [DatabaseItem] -> Integer
sumDb' = foldr getSum 0
  where getSum (DbNumber number) acc = (+) number acc
        getSum _                 acc = acc

-- 5. Function that gets the average of the DbNumber values. Use `fromIntegral` to get from Integer to Double
avgDb :: [DatabaseItem] -> Double
avgDb xs = (/) (fromIntegral (sumDb xs)) (fromIntegral (length (filterDbNumber xs)))

avgDb' :: [DatabaseItem] -> Double
avgDb' xs = (/) sumTotal countTotal
  where sumTotal   = fromIntegral $ sumDb xs
        countTotal = fromIntegral $ length $ filterDbNumber xs


main = do
  print $ filterDbDate    theDatabase
  print $ filterDbDate'   theDatabase
  print $ filterDbNumber  theDatabase
  print $ filterDbNumber' theDatabase
  print $ mostRecent      theDatabase
  print $ sumDb           theDatabase
  print $ sumDb'          theDatabase
  print $ avgDb           theDatabase
  print $ avgDb'          theDatabase
