getInt :: IO Int
getInt = fmap read getLine

-- these two are more verbose, better to use fmap over IO

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)
