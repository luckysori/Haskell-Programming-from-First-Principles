foldAny :: (a -> Bool) -> [a] -> Bool
foldAny cond = foldr ((||) . cond) False
