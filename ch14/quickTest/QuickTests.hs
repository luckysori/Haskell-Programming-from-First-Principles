-- pg 878

module QuickTests where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

-- 1.

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

propHalfIdentity :: (Eq a, Fractional a) => a -> Bool
propHalfIdentity x = halfIdentity x == x

checkHalf :: IO ()
checkHalf = quickCheck (propHalfIdentity :: Float -> Bool)

-- 2.

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

checkSort :: IO ()
checkSort = quickCheck $ (listOrdered :: [Int] -> Bool) . sort

-- 3.

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

checkAssCommSum :: IO ()
checkAssCommSum = do
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int -> Bool)

-- 4.

timesAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
timesAssociative x y z =
  x * (y * z) == (x * y) * z

timesCommutative :: (Num a, Eq a) => a -> a -> Bool
timesCommutative x y =
  x * y == y * x

checkAssCommMult :: IO ()
checkAssCommMult = do
  quickCheck (timesAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (timesCommutative :: Int -> Int -> Bool)

-- 5.

quotRemRel :: Integral a => (a, a) -> Bool
quotRemRel (x, y) = (quot x y)*y + (rem x y) == x

divModRel :: Integral a => (a, a) -> Bool
divModRel (x, y) = (div x y)*y + (mod x y) == x

genNonZeroInt :: Gen (Int, Int)
genNonZeroInt = (arbitrary :: Gen (Int, Int))
                `suchThat`
                (\(x, y) -> (x /= 0) && (y /= 0))
  
checkDivQuot :: IO ()
checkDivQuot = do
  quickCheck $ forAll genNonZeroInt $ quotRemRel
  quickCheck $ forAll genNonZeroInt $ divModRel

-- 6.

powerOfAssociative :: (Num a, Eq a, Integral b, Integral c) => a -> b -> c -> Bool
powerOfAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerOfCommutative :: (Integral a, Eq a) => a -> a -> Bool
powerOfCommutative x y = x ^ y == y ^ x

checkAssCommPower :: IO ()
checkAssCommPower = do
  quickCheck (powerOfAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (powerOfCommutative :: Int -> Int -> Bool)

-- 7.

revRevIsId :: Eq a => [a] -> Bool
revRevIsId xs = (reverse (reverse xs)) == (id xs)

checkRevSquared :: IO ()
checkRevSquared = quickCheck (revRevIsId :: [Int] -> Bool)

-- 8.

dollar :: Eq b => (a -> b) -> a -> Bool
dollar f x = (f $ x) == (f x)

composition :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
composition f g x = ((f . g) x) == (f (g x))

-- 9.

-- Fails
compFs1 :: Eq a => [a] -> [a] -> Bool
compFs1 xs ys = (foldr (:) xs ys) == ((++) xs ys)

-- Passes
compFs1Mod :: Eq a => [a] -> [a] -> Bool
compFs1Mod xs ys = (foldr (:) xs ys) == ((flip (++)) xs ys)

compFs2 :: Eq a => [[a]] -> Bool
compFs2 xs = (foldr (++) [] xs) == (concat xs)

checkFs :: IO ()
checkFs = do
  quickCheck (compFs1 :: [Int] -> [Int] -> Bool)
  quickCheck (compFs1Mod :: [Int] -> [Int] -> Bool)
  quickCheck (compFs2 :: [[Int]] -> Bool)

-- 10.

takeLength :: Eq a => Int -> [a] -> Bool
takeLength n xs = length (take n xs) == n

-- Fails, because n can be larger than the number of elements of xs
checkTakeLength :: IO ()
checkTakeLength = quickCheck (takeLength :: Int -> [Int] -> Bool)

-- 11.

readShow :: (Show a, Read a, Eq a) => a -> Bool
readShow x = (read (show x)) == x

checkReadShow :: IO ()
checkReadShow = quickCheck (readShow :: Int -> Bool)

-- failure of squareIdentity
square x = x * x

-- this property doesn't hold, even though it's mathematically correct, because sqrt only obtains an approximation to a certain degree of accuracy to the solution, particularly when the solution is an irrational number
squareIdentity = square . sqrt

-- idempotence

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = ((toUpper x):xs) 

f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

f' x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

checkIdem :: IO ()
checkIdem = do
  quickCheck f
  quickCheck (f' :: [Int] -> Bool)

-- generators

data Fool = Fulse | Frue deriving (Eq, Show)

-- choose doesn't work, bc it expects a type with an instance of random
genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genMoreFulse :: Gen Fool
genMoreFulse = frequency [(2, return Fulse), (1, return Frue)]
