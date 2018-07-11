splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy c (x:xs)
  | c == x = splitBy c xs
splitBy c xs = [takeWhile (/=c) xs] ++ (splitBy c (dropWhile (/=c) xs))

myWords :: String -> [String]
myWords = splitBy ' '

myLines :: String -> [String]
myLines = splitBy '\n'
