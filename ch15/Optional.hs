module Optional where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada         = Nada
  mappend Nada (Only x)     = Only (mappend mempty x)
  mappend (Only x) Nada     = Only (mappend x mempty)
  mappend (Only x) (Only y) = Only (mappend x y)
