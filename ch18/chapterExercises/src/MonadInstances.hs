module MonadInstances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- 2.

data PhhhbbtttEither b a = Left' a | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a)  = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' a1) (Left' a2) = Left' (a1 a2)
  (<*>) (Right' b) _          = Right' b
  (<*>) _ (Right' b)          = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' x) f  = f x
  (>>=) (Right' x) _ = Right' x

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' b, Right' a]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3.

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure = flip Cons Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) xs ys = flatMap (\x -> fmap x ys) xs

instance Monad List where
  return = pure
  (>>=) xs f = concat' $ fmap f xs

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
  | m == 0 = return Nil
  | m > 6 = arbitraryList 6
  | otherwise = Cons <$> arbitrary <*> (arbitraryList (m-1))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $ applicative (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $
    functor (Right'
             (True, False, True) :: PhhhbbtttEither
              (Bool, Bool, Bool)
              (Int, Int, Int))
  quickBatch $
    applicative (Right'
                 (True, False, True) :: PhhhbbtttEither
                  (Bool, Bool, Bool)
                  (Int, Int, Int))
  quickBatch $
    monad (Right'
           (True, False, True) :: PhhhbbtttEither
            (Bool, Bool, Bool)
            (Int, Int, Int))
  quickBatch $ functor (Identity (True, False, True))
  quickBatch $ applicative (Identity (True, False, True))
  quickBatch $ monad (Identity (True, False, True))
  quickBatch $ functor (Cons (True, False, True) Nil)
  quickBatch $ applicative (Cons (True, False, True) Nil)
  quickBatch $ monad (Cons (True, False, True) Nil)
