newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"
consonants = [ x | x <- ['a'..'z'], not (elem x vowels)]

mkWord :: String -> Maybe Word'
mkWord s
  | c >= v = Just (Word' s)
  | otherwise = Nothing
  where
    c = length $ filter (\x -> elem x consonants) s
    v = length $ filter (\x -> elem x vowels) s    

