-- pg 738

-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just x) = f x

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x id y

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x y -> (maybeToList x) ++ y) []

-- 6.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = go ms []
  where
    go [] xs = Just xs
    go ((Just y): ys) xs = go ys (xs ++ [y])
    go _ _ = Nothing
