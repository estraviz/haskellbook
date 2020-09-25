module DatatypeDeclaration where

data Rocks = 
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- Ex.1
phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

-- Ex.2
truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- Ex.3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Ex.4 - Error as Papu has no instance of Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

