-- pg 604

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

{-

1. Doggies is a type constructor
2. Kind of Doggies: * -> *
3. Kind of Doggies String: *
4. Type Husky 10 : Num a => Doggies a
5. Type Husky 10 (10 :: Integer): Doggies Integer
6. Type Mastiff "Scooby Doo": Doggies [Char]
7. DogueDeBordeaux is both a type constructor and a data constructor
8. Type DogueDeBordeaux: a -> DogueDeBordeaux a
9. Type DogueDeBordeaux "doggie!": DogueDeBordeaux [Char]

-}
