foldMaxBy :: (a -> a -> Ordering) -> [a] -> a
foldMaxBy f (x:xs) = foldr
                     (\x y -> case f x y of
                                GT -> x
                                _ -> y
                     )
                     x
                     (x:xs)
