import           Control.Monad          (liftM)
import           Control.Monad.IO.Class
import           MyReaderT
import           MyStateT

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (<*>) (MaybeT f) (MaybeT mma) =
    MaybeT $ (<*>) <$> f <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) (MaybeT mma) amb = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a  -> runMaybeT (amb a)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = MaybeT . (liftM Just) . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . (\x r -> liftIO x)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO ma = StateT $ \s -> liftM (\x -> (,) x s) (liftIO ma)
