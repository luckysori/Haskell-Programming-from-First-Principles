splitAtSpace :: [Char] -> [[Char]]
splitAtSpace [] = []
splitAtSpace (' ':xs) = splitAtSpace xs
splitAtSpace xs = [takeWhile (/=' ') xs] ++ splitAtSpace (dropWhile (/=' ') xs)
