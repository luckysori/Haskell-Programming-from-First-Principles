--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show

--data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String
--data Author = Author (AuthorName, BookType)

-- Example of normal form or sum of products.
data Author = Fiction AuthorName | Nonfiction AuthorName deriving (Eq, Show)

-- pg 642

{-data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show-}

type Gardener = String

--data Garden = Garden Gardener FlowerType deriving Show

-- 1. Sum of products normal form of Garden:

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
