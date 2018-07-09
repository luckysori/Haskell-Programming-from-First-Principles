foldOr :: [Bool] -> Bool
foldOr = foldr (||) False
