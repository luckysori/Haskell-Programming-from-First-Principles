import           Control.Monad.Trans.Reader

rDec :: Num a => Reader a a
rDec = reader $ \r -> (r - 1)

rDec' :: Num a => Reader a a
rDec' = reader $ subtract 1


