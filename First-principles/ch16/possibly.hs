data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)
  
{-
A Functor instance that applies only to First is impossible
because to do so the instance would have to be on Sum, whose
kind is * -> * -> *, which is no the needed * -> *
-}
