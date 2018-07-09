module ChapterExercises where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Specialized types
-- 1.

listPure :: a -> [a]
listPure = pure

listApply :: [(a -> b)] -> [a] -> [b]
listApply = (<*>)

-- 2.

ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3.

tuplePure :: Monoid a => b -> (a, b)
tuplePure = pure

tupleApply :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
tupleApply = (<*>)

-- 4.

functionPure :: a -> (e -> a)
functionPure = pure

functionApply :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
functionApply = (<*>)

-- Type instances
-- 1.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- 2.

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a1 b1) (Two a2 b2) = Two (mappend a1 a2) (b1 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a1 b1 c1) (Three a2 b2 c2) = Three
                                            (mappend a1 a2)
                                            (mappend b1 b2)
                                            (c1 c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x y z) (Three' a b c) = Three'
                                        (mappend x a)
                                        (y b)
                                        (z c)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c d) (Four w x y z) = Four
                                        (a <> w)
                                        (b <> x)
                                        (c <> y)
                                        (d z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c d) (Four' w x y z) = Four'
                                        (a <> w)
                                        (b <> x)
                                        (c <> y)
                                        (d z)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations, pg 1185

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\x y z -> (x, y, z))

main :: IO ()
main = do
  quickBatch $ applicative $ Pair (True, False, True) (True, False, True)
  quickBatch $ applicative $ Two ("", "lol", "haha") (True, False, True)
  quickBatch $ applicative $ Three ("", "lol", "haha") ("", "lol", "haha") (True, False, True)
  quickBatch $ applicative $ Three' ("", "lol", "haha") (True, False, True) (True, False, True)
  quickBatch $ applicative $ Four ("", "lol", "haha") ("", "lol", "haha") ("", "lol", "haha") (True, False, True)
  quickBatch $ applicative $ Four' ("", "lol", "haha") ("", "lol", "haha") ("", "lol", "haha") (True, False, True)
