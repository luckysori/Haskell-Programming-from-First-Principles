mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mTh''' = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC x y =
  case x > y of
    True -> x
    _ -> y

ifEvenAdd2 n =
  case even n of
    True -> (+) n 2
    _ -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1


-- arith2.hs

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

h :: String -> [String]
h = undefined
i :: a -> a
i a = a

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

hunsD x = d2
  where xLast = x `div` 100
        d2 = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y cond =
  case cond of
    True -> x
    _ -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y cond
  | cond = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g a2b (a, c) = (a2b a, c) 

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print ((roundTrip 4) :: Int)
  print (id 4)
