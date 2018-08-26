newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

readerTMaybe :: ReaderT Int Maybe Int
readerTMaybe = ReaderT $ \r -> if even r then Just 1 else Nothing

maybeTReader :: MaybeT ((->) Int) Int
maybeTReader = MaybeT $ \r -> if even r then Just 1 else Nothing

main :: IO ()
main = print $ and $ fmap (\x -> (==) ((runMaybeT maybeTReader) x) ((runReaderT readerTMaybe) x)) [1..10000]
