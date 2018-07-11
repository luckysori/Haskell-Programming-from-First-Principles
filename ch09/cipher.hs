module Cipher where

import Data.Char
import Test.QuickCheck

caesar :: Int -> String -> String
caesar _ [] = []
caesar s (x:xs)
  | isLower x = chr (((((ord x) + s) - 97) `mod` 26) + 97) : caesar s xs
  | otherwise = chr (((((ord x) + s) - 65) `mod` 26) + 65) : caesar s xs

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar s (x:xs)
  | isLower x = chr (((((ord x) - s) - 97) `mod` 26) + 97) : unCaesar s xs
  | otherwise = chr (((((ord x) - s) - 65) `mod` 26) + 65) : unCaesar s xs

main :: IO ()
main = do
  putStr "Message to cypher: "
  msg <- getLine
  putStr "Shift by: "
  s   <- readLn
  putStrLn (caesar (s :: Int) msg)

genLetter :: Gen Char
genLetter = elements (['a'..'z'] ++ ['A'..'Z'])

genWord :: Gen String
genWord = listOf genLetter
  
propEncodeDecode :: String -> Int -> Bool
propEncodeDecode xs s = (==) xs $ unCaesar s $ caesar s xs 

testMain :: IO ()
testMain = quickCheck $ forAll genWord $ propEncodeDecode
