-- pg 626

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)
type Pigs = Int

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- 1.

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2.

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

-- 3. If both instances are available they overlap, so it compiles but it can't be decided which one to use during execution

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (x, y) = tooMany (x + y)
