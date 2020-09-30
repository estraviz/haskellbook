module TypeCheck where

-- Ex.1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- Ex.2
data Mood = Blah 
          | Woot deriving (Show, Eq)

-- instance Eq Mood where
--   (==) Blah Blah = True
--   (==) Woot Woot = True
--   (==) _ _       = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot
               then Blah
               else x

-- Ex.4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving Show 
data ShortSentence = ShortSentence Subject Verb deriving Show

s1 :: ShortSentence 
s1 = ShortSentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

