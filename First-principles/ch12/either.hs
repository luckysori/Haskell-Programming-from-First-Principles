-- pg 740

-- 1.

takeLefts :: Either a b -> [a] -> [a]
takeLefts (Left x) ys = x : ys
takeLefts _ ys = ys

lefts' :: [Either a b] -> [a]
lefts' = foldr takeLefts []

-- 2.

takeRights :: Either a b -> [b] -> [b]
takeRights (Right x) ys = x : ys
takeRights _ ys = ys

rights' :: [Either a b] -> [b]
rights' = foldr takeRights []

-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = (Just (f x))

-- 5.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\y -> Just (f y))
