-- pg 513

-- 1.

filterMult3 = filter (\x -> (rem x 3) == 0) [1..30]

-- 2.

countMult3 = length . filter (\x -> (rem x 3) == 0) $ [1..30]

-- 3.

myFilter = filter (\x -> not $ elem x ["the", "a", "an"]) . words
