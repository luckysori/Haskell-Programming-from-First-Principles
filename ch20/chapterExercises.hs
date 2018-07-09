import           Data.Maybe

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = mappend (f b1) (f b2)

data Four' a b = Four' a b b b deriving Show

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) =
    foldr mappend mempty [f b1, f b2, f b3]

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap toApplicative
  where toApplicative x
          | f x = pure x
          | otherwise = mempty
