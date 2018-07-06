foldMap' :: (a -> b) -> [a] -> [b]
foldMap' f = foldr (\x y -> (f x) : y) []
