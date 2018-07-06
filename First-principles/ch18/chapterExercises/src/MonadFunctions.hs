module MonadFunctions where

import           Control.Monad

-- 1.

j :: Monad m => m (m a) -> m a
j = join

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f)

-- 6.

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
