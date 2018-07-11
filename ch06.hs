import Data.List (sort)

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving Show

data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int)
       (TisAn int') =
    int == int'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two leftInt rightInt)
       (Two leftInt' rightInt') =
    leftInt == leftInt'
    && rightInt == rightInt'

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') =
    int == int'
  (==) (TisAString str) (TisAString str') =
    str == str'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair t u) (Pair t' u') =
    t == t'
    && u == u'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x'
    && y == y'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') =
    x == x'
  (==) (ThatOne x) (ThatOne x') =
    x == x'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) =
    x == y
  (==) (Goodbye x) (Goodbye y) =
    x == y
  (==) _ _ = False

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String deriving (Eq, Show, Ord)
data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)
data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a2b a b = (a2b a) == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith a2b int a = (a2b a) + (fromInteger int)


newtype Nada =
  Nada Double deriving (Eq, Show, Num)
instance Fractional Nada where
  (Nada x) / (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)
