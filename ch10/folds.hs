-- pg 557

-- 1. foldr (*) 1 [1..5] ~ b) foldl (flip (*)) 1 [1..5] and c) foldl (*) 1 [1..5]

-- 2.

{-

foldl (flip (*)) 1 [1..3] =
(3 * (2 * (1 * 1))) =
(3 * (2 * 1)) =
(3 * 2) =
6

-}

-- 3. Difference between foldr and foldl: foldr, but not foldl, associates to the right

-- 4. Folds, being catamorphisms, are generally used to reduce structure

-- 5.

{-

a) foldr (++) [] ["woot", "WOOT", "woot"]
b) foldr max ' ' "fear is the little death"
c) foldr (&&) True [False, True]
d) foldr (||) False [False, True]. The zero has to be False because otherwise the expression always evaluates to True.
e)

foldl ((++) . show) "" [1..5] =
((++) . show) ((++) . show) ((++) . show) ((++) . show) ((++) . show) "" 1) 2) 3) 4) 5)

Fix: either foldr ((++) . show) "" [1..5] or foldl (flip ((++) . show)) "" [1..5], but the second one results in a reversed list

f) foldr (flip const) 'a' [1..5]. The type signature of foldr requires a function of type (a -> b -> b) but const is (b -> a -> b).

g) foldr (flip const) 0 "tacos". Same as f)

h) foldl const 0 "burritos". The type signature of foldl requires a function of type (b -> a -> b) but (flip const) is (a -> b -> b)

i) foldl const 'z' [1..5]. Same as h)

-}
