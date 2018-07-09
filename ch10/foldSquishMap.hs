foldSquishMap :: (a -> [b]) -> [a] -> [b]
foldSquishMap f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = foldSquishMap id
