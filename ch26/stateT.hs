{-# LANGUAGE InstanceSigs #-}

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ \s -> fmap (\(x, y) -> (f x, y)) (smas s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (<*>) (StateT smabs) (StateT smas) =
    StateT $ \s -> do
    abs <- smabs s
    as <- smas s
    return $ ((fst abs) (fst as), s)

instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f =
    StateT $ \s -> do
    (a, _) <- smas s
    runStateT (f a) s
