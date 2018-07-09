data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom
  | (num*denom) > 0 =
    Result (go (abs num) (abs denom) 0)
  | otherwise =
    Result (-(go (abs num) (abs denom) 0))
  where go n d count
          | n < d = count
          | otherwise = go (n - d) d (count + 1)
