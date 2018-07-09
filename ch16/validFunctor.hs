import GHC.Arr

-- 1. No valid functor for Bool, since it's all value,
-- no structure

-- 2.

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4. No valid functor for Mu since its kind is (* -> *) -> *.
-- Nor is there a valid functor for Mu f, since its kind is *.

newtype Mu f = InF { outF :: f (Mu f) }

-- 5. No valid functor for D since its kind is *.

data D = D (Array Word Word) Int Int
