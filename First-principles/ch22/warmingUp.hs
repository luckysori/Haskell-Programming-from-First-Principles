import           Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  return $ (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= (\x -> cap >>= (\y -> return (x, y)))
