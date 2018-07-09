-- pg 944

module Semigroups where

import Test.QuickCheck
import Data.Monoid as M
import Data.Semigroup as S

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)
  
instance Arbitrary Trivial where
  arbitrary = return Trivial
  
semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty M.<> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a M.<> mempty) == a

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x S.<> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity (mempty :: Monoid a => a)
  mappend = (S.<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdAssoc =
  Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two w z) = Two (x S.<> w) (y S.<> z)

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = Two (mempty :: Monoid a => a) (mempty :: Monoid b => b)
  mappend = (S.<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc =
  Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

-- 4.

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a S.<> d) (b S.<> e) (c S.<> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc =
  Three (Sum Int) (Product Int) (Sum Int) -> Three (Sum Int) (Product Int) (Sum Int) -> Three (Sum Int) (Product Int) (Sum Int) -> Bool

-- 5.

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four
                                     (a S.<> e)
                                     (b S.<> f)
                                     (c S.<> g)
                                     (d S.<> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc =
  Four (Sum Int) (Product Int) (Product Int) (Sum Int) -> Four (Sum Int) (Product Int) (Product Int) (Sum Int) -> Four (Sum Int) (Product Int) (Product Int) (Sum Int) -> Bool

-- 6.

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = (BoolConj True)
  mappend = (S.<>)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- 6.

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (S.<>)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  Fst _ <> Snd x = Snd x
  _ <> Fst x = Fst x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc =
  Or Int Char -> Or Int Char -> Or Int Char -> Bool

-- 9.

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  c1 <> c2 = Combine (\n -> ((unCombine c1) n) S.<> ((unCombine c2) n))

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine $ (\_ -> mempty)
  mappend = (S.<>)
  
genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
  x <- (arbitrary :: (CoArbitrary a, Arbitrary b) => Gen (a -> b))
  return (Combine x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = genCombine

instance Show (Combine a b) where
  show _ = "Combine arbFunc"

semigroupCombineAssoc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
semigroupCombineAssoc m1 m2 m3 a =
  unCombine (m1 S.<> (m2 S.<> m3)) a == unCombine ((m1 S.<> m2) S.<> m3) a

monoidCombineRightId :: (Eq m, Semigroup m, Monoid m) => Combine a m -> a -> Bool
monoidCombineRightId m a =
  unCombine (mappend m mempty) a == (unCombine m) a

monoidCombineLeftId :: (Eq m, Semigroup m, Monoid m) => Combine a m -> a -> Bool
monoidCombineLeftId m a =
  unCombine (mappend mempty m) a == (unCombine m) a

type CombAssoc =
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Int ->
  Bool

-- 10.

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  c1 <> c2 = Comp ((unComp c1) . (unComp c2))

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (S.<>)

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  x <- arbitrary
  return (Comp x)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genComp

instance Show (Comp a) where
  show _ = "Comp arbFunc"

semigroupCompAssoc :: Eq m => Comp m -> Comp m -> Comp m -> m -> Bool
semigroupCompAssoc m1 m2 m3 a =
  unComp (m1 S.<> (m2 S.<> m3)) a == unComp ((m1 S.<> m2) S.<> m3) a

monoidCompRightId :: Eq m => Comp m -> m -> Bool
monoidCompRightId m a =
  unComp (mappend m mempty) a == (unComp m) a

monoidCompLeftId :: Eq m => Comp m -> m -> Bool
monoidCompLeftId m a =
  unComp (mappend mempty m) a == (unComp m) a

type CompAssoc =
  Comp Int ->
  Comp Int ->
  Comp Int ->
  Int ->
  Bool

-- 11.

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure' x) <> (Failure' y) = Failure' (x S.<> y)
  (Failure' _) <> (Success' x) = Success' x
  (Success' x) <> _ = Success' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Success' a, Failure' b]

type ValidAssoc =
  Validation String Int ->
  Validation String Int ->
  Validation String Int ->
  Bool

newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> ((mempty :: Monoid a => a), s)
  mappend m1 m2 =
    Mem $ \s -> ((fst ((runMem m1) s)) M.<> (fst ((runMem m2) s)),
                  snd ((runMem m1) (snd ((runMem m2) s))))

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    x <- arbitrary
    return (Mem x)

instance Show (Mem s a) where
  show _ = "Mem arbFunc"
  
memRightId :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memRightId m s =
  runMem (mappend m mempty) s == (runMem m) s

memLeftId :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
memLeftId m s =
  runMem (mappend mempty m) s == (runMem m) s

memAssoc :: (Eq s, Eq a, Monoid a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
memAssoc m1 m2 m3 s =
  runMem (mappend m1 (mappend m2 m3)) s == runMem (mappend (mappend m1 m2) m3) s

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupCombineAssoc :: CombAssoc)
  quickCheck (semigroupCompAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidAssoc)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) (Product Int) -> Bool)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) (Product Int) -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidCombineRightId :: Combine Int (Sum Int) -> Int -> Bool)
  quickCheck (monoidCombineLeftId :: Combine Int (Sum Int) -> Int -> Bool)  
  quickCheck (monoidCompRightId :: Comp Int -> Int -> Bool)
  quickCheck (monoidCompLeftId :: Comp Int -> Int -> Bool)  
  quickCheck (memRightId :: Mem Int String -> Int -> Bool)
  quickCheck (memLeftId :: Mem Int String -> Int -> Bool)
  quickCheck
    (memAssoc :: Mem Int String -> Mem Int String -> Mem Int String -> Int -> Bool)
