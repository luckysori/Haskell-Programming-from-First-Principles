{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Ord, Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = fmap Yep $ f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

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

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = mappend (f a) (foldMap f as)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = (fmap Cons $ f a ) <*> (traverse f as)

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
  | m == 0 = return Nil
  | m > 6 = arbitraryList 6
  | otherwise = Cons <$> arbitrary <*> (arbitraryList (m-1))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = arbitrary
                >>=
                  (\x -> arbitrary
                    >>=
                      (\y -> arbitrary
                        >>=
                          (\z -> return $ Three x y z)))

-- instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
--          Arbitrary (Three a b c) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     c <- arbitrary
--     return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = arbitrary >>= (\x -> arbitrary >>= (\y -> return $ Pair x y))

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = mappend (f b1) (f b2)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> (f b1) <*> (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Big a b1 b2

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b1 b2 b3) = mappend (mappend (f b1) (f b2)) (f b3)

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> (f b1) <*> (f b2) <*> (f b3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return $ Bigger a b1 b2 b3

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
--   (S x y) =-= (S p q) =
--         (property $ (=-=) <$> x <*> p)
--     .&. (y =-= q)

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = mappend (foldMap f n) (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> (traverse f n) <*> (f a)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty               = Empty
  fmap f (Leaf a)            = Leaf $ f a
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) `mappend`
                                  (f a) `mappend`
                                  (foldMap f right)
  foldr _ b Empty               = b
  foldr f b (Leaf a)            = f a b
  foldr f b (Node left a right) = f a (foldr f (foldr f b left) right)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$>
                                   (traverse f left) <*>
                                   (f a) <*>
                                   (traverse f right)

arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree 0 = fmap Leaf arbitrary
arbitraryTree n = frequency
                    [(1, return Empty)
                    ,(1, fmap Leaf arbitrary)
                    ,(2, Node <$> arbitraryTree (n - 1)
                         <*> arbitrary
                         <*> arbitraryTree (n - 1))]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbitraryTree

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable $ Identity (True, False, [True])
  quickBatch $ traversable $ (Constant (True, False, [True]) :: Constant (Bool, Bool, [Bool]) (Bool, Bool, [Bool]))
  quickBatch $ traversable $ Yep (True, False, [True])
  quickBatch $ traversable $ Cons (True, False, [True]) Nil
  quickBatch $ traversable $ Three (True, False, [True]) (True , False, [True]) (True , False, [True])
  quickBatch $ traversable $ Pair (True, False, [True]) (True , False, [True])
  quickBatch $ traversable $ Big (True, False, [True]) (True , False, [True]) (True , False, [True])
  quickBatch $ traversable $ Bigger (True, False, [True]) (True , False, [True]) (True , False, [True]) (True, False, [True])
  quickBatch $ traversable $ S [] (True, False, [True])
  quickBatch $ traversable $ Leaf (True, False, [True])
