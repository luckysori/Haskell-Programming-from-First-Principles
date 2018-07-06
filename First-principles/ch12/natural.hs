-- pg 736

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ ns) = 1 + natToInteger ns

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i <  0 = Nothing
  | i == 0 = Just Zero
  | otherwise = go (Succ Zero) (i - 1)
  where
    go n 0 = Just n
    go n j = go (Succ n) (j - 1)

