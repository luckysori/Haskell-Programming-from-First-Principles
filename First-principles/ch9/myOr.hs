myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x = x
  | otherwise = myOr xs
