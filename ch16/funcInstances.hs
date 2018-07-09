-- {-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorId' :: (Eq (f a), Functor f) => f a -> Bool
functorId' x = fmap id x == id x
  
functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1.

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

-- 2.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

-- 5.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

-- 6.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return (Four x y z w)

-- 7.

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z w) = Four' x y z (f w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return (Four' x y z w)

{-
8. An instance of the functor typeclass makes no sense for
data Trivial = Trivial, since it is just value, so it's kind is *
and there is no structure to keep intact.
-}



main :: IO ()
main = do
  quickCheck (functorId' :: Identity Int -> Bool)
  quickCheck (functorCompose' :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Pair Int -> Bool)
  quickCheck (functorCompose' :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Two Char Int -> Bool)
  quickCheck (functorCompose' :: Two Char Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Three Int Char Int -> Bool)
  quickCheck (functorCompose' :: Three Int Char Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Three' Char Int -> Bool)
  quickCheck (functorCompose' :: Three' Char Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Four Int Int Char Int -> Bool)
  quickCheck (functorCompose' :: Four Int Int Char Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorId' :: Four' Char Int -> Bool)
  quickCheck (functorCompose' :: Four' Char Int -> Fun Int Int -> Fun Int Int -> Bool)
  
