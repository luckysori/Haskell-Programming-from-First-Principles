import           Data.Monoid
import           Optional
import           Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) (First' (Only _)) = First' (Only x)
  mappend (First' (Only x)) _                 = First' (Only x)
  mappend _ (First' (Only x))                 = First' (Only x)
  mappend _ _                                 = First' (Nada)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  x <- arbitrary
  frequency [(3, return (First' (Only x))), (1, return (First' (Nada)))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
