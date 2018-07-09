data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Or a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x
