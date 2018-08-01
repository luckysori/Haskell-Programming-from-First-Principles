module Main where

import           Control.Monad
import           Control.Monad.Trans.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
  -- same as lift ma = EitherT $ liftM Right ma

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> liftM (\x -> (,) x s) ma

main :: IO ()
main = undefined
