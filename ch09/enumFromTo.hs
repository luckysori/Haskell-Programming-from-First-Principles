enumFromTo' :: Enum a => a -> a -> [a]
enumFromTo' x y
  | fromEnum x > fromEnum y = []
  | otherwise = [x] ++ (enumFromTo' (succ x) y)

eftBool :: Bool -> Bool -> [Bool]
eftBool = enumFromTo'

eftOrd :: Ordering
  -> Ordering
  -> [Ordering]
eftOrd = enumFromTo'

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo'

eftChar :: Char -> Char -> [Char]
eftChar = enumFromTo'
