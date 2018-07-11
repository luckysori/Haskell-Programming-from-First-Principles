myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go xs x
  where go [] top = top
        go (y:ys) top
          | (f top y) == LT = go ys y
          | otherwise = go ys top

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
