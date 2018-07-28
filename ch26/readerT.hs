newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \_ -> pure x

  (ReaderT f) <*> (ReaderT a) = ReaderT $ (<*>) <$> f <*> a

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ \r -> rma r >>= \a -> runReaderT (f a) r
