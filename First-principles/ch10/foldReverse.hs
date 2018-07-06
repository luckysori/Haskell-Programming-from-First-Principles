foldReverse :: [a] -> [a]
foldReverse = foldl (flip (:)) []
