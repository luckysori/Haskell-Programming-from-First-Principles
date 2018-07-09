-- Pg 618

data Example = MakeExample deriving Show

data Thing = Thing Int deriving Show

-- 1. Type of MakeExample is Example. When requesting Example's type, an error is given because it is not a data constructor, but rather a type constructor

-- 2. :info Example gives information on the type Example, including its type declaration and typeclass instances.

-- 3. Type of Thing is Int -> Thing
