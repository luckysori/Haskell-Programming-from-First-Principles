module Validation where

import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (Failure' e1) <*> (Failure' e2) = Failure' (mappend e1 e2)
  (Failure' e) <*> _ = Failure' e
  _ <*> (Failure' e) = Failure' e
  (Success' a1) <*> (Success' a2) = Success' (a1 a2)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = quickBatch $ applicative (Success' ("a", "b", "c") :: Validation String (String, String, String))
