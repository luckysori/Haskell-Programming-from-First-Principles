{- 2^3 implementations

convert1 :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert2 :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = False

convert3 :: Quantum -> Bool
convert Yes = True
convert No = False
convert Both = True

convert4 :: Quantum -> Bool
convert Yes = False
convert No = True
convert Both = True

convert5 :: Quantum -> Bool
convert Yes = True
convert No = False
convert Both = False

convert6 :: Quantum -> Bool
convert Yes = False
convert No = False
convert Both = True

convert7 :: Quantum -> Bool
convert Yes = False
convert No = True
convert Both = False

convert8 :: Quantum -> Bool
convert Yes = False
convert No = False
convert Both = False

-}

{-

1. 4 + 4 = 8
2. 4*4 = 16
3. 4^4 = 256
4. 2*2*2 = 8
5. 2^(2*2) = 16, function with two arguments
6. 4^(2*4) = 65536, function with two arguments

-}
