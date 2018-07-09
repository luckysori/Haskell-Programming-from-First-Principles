module ListAndZipListApplicative where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

{-instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary-}

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

-- List Applicative Exercise

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

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

-- arbitraryList copied from http://codingstruggles.com/haskell/arbitrary-length-lists-quickcheck.html

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
  | m == 0 = return Nil
  | m > 6 = arbitraryList 6
  | otherwise = Cons <$> arbitrary <*> (arbitraryList (m-1))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 _           = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y)
                                     (zipWith' f xs ys)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  (<*>) (ZipList' xs) (ZipList' ys) =
    ZipList' (zipWith' ($) xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- (arbitrary :: Arbitrary a => Gen (List a))
    return $ ZipList' a

main :: IO ()
main = do
  quickBatch $ applicative (Cons (True, False, False) Nil)
  quickBatch $ applicative (ZipList' (Cons (True, False, False) Nil))
