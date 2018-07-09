-- pg 694
import Data.Char

-- 1.

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xxs@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xxs ys

-- 2.

--capitalize :: String -> String
--capitalize (x:xs) = (toUpper x) : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = go (words sentence)
  where
    go [] = []
    go (x@(y:ys):xs) = (x, (toUpper y) : ys) : go xs
