myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go xs x
  where go [] bot = bot
        go (y:ys) bot
          | (f bot y) == GT = go ys y
          | otherwise = go ys bot

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
