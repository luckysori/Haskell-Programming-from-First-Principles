-- pg 733

-- 1.

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = go . words
  where
    go (x:[]) = case notThe x of
                  (Just y) -> y
                  Nothing  -> "a"
    go (x:xs) = case notThe x of
                  (Just y) -> y ++ " " ++ (go xs)
                  Nothing  -> "a " ++ (go xs)

-- 2.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go ("the":[]) = 0
    go ("the":(x:xs))
      | elem (head x) "aeiou" = 1 + go xs
      | otherwise = go (x:xs)
    go (_:xs) = go xs

-- 3.

countVowels :: String -> Integer
countVowels word = toInteger . length $ [ x | x <- word, elem x "aeiou"]
