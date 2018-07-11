myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x = True
  | otherwise = myAny f xs
