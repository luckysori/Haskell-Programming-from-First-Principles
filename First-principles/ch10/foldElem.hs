foldElem :: Eq a => a -> [a] -> Bool
foldElem x = foldr ((||) . (x==)) False

elem' :: Eq a => a -> [a] -> Bool
elem' x = any (==x)
