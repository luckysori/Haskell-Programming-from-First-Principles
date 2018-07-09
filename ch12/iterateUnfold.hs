-- pg 744

-- 1.

myIterate :: (a -> a) -> a -> [a]
myIterate f b = b : (myIterate f (f b))

-- 2.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing -> []
                  (Just (a,b)) -> a : (myUnfoldr f b)

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x

                  
