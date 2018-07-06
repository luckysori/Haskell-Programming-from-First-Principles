module Reverse where

rvrs :: String -> String
rvrs xs = (++) (drop 9 xs) $ (++) (drop 5 $ take 9 xs) (take 5 xs)

main :: IO ()
main = print $ rvrs "Curry is awesome"
