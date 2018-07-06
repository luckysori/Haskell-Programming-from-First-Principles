foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter f = foldr
               (\x y -> case f x of
                          True -> x : y
                          _    -> y
               )
               []
