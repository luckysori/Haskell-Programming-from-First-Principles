-- pg 695

-- 1.

import Data.Char

capitalize :: String -> String
capitalize (x:xs) = (toUpper x) : xs

-- 2. I don't love this solution, but it works

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph (' ':xs) = ' ' : capitalizeParagraph xs
capitalizeParagraph (x:xs) = (toUpper x) : go xs
  where
    go [] = []
    go (x:[]) = [x]
    go (x:(y:([]))) = x : (y : [])
    go (x:(xs@(y:(z:zs))))
      | x == '.' = x : (y : (toUpper z) : go zs)
      | otherwise = x : go xs
