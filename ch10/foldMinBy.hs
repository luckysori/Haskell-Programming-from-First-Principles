foldMinBy :: (a -> a -> Ordering) -> [a] -> a
foldMinBy f (x:xs) = foldr
                     (\x y -> case f x y of
                                GT -> y
                                _ -> x
                     )
                     x
                     (x:xs)
