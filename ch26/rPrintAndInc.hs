import           Control.Monad.Trans.Reader

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
  putStrLn $ "Hi: " ++ show x
  return $ x + 1
