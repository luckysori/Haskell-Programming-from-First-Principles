-- pg 451

-- 1.

{- 
   dividedBy 15 2 =
   go 15 2 0 =
   go 13 2 1 =
   go 11 2 2 =
   go 9 2 3 =
   go 7 2 4 =
   go 5 2 5 =
   go 3 2 6 =
   go 1 2 7 =
   (7, 1)
-}

-- 2.

recSum :: (Eq a, Num a) => a -> a
recSum 1 = 1
recSum n = n + recSum (n-1)

-- 3.

recMult :: (Integral a) => a -> a -> a
recMult x 1 = x
recMult x y = x + recMult x (y-1)
