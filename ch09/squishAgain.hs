import SquishMap

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> []++x++[])
