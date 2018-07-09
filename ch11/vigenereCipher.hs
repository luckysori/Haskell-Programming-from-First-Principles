module VigenereCipher where

import Data.Char
import Test.QuickCheck

shift :: String -> Int -> Int
shift word i
  | isLower letter = (ord letter - 97)
  | isUpper letter = (ord letter - 65)
  | otherwise = error ("Codeword contains forbidden character: " ++ [letter])
  where letter = word !! (i `mod` (length word))

caesarV :: String -> String -> String
caesarV codeword msg = go codeword 0 msg
  where
    go _ _ [] = []
    go codeword n (x:xs)
      | isLower x =
        chr (((((ord x) + (shift codeword n)) - 97) `mod` 26) + 97) :
        go codeword (n + 1) xs
      | isUpper x =
        chr (((((ord x) + (shift codeword n)) - 65) `mod` 26) + 65) :
        go codeword (n + 1) xs
      | otherwise =
        x :
        go codeword n xs

unCaesarV :: String -> String -> String
unCaesarV codeword msg = go codeword 0 msg
  where
    go _ _ [] = []
    go codeword n (x:xs)
      | isLower x =
          chr (((((ord x) - (shift codeword n)) - 97) `mod` 26) + 97) :
          go codeword (n + 1) xs
      | isUpper x =
          chr (((((ord x) - (shift codeword n)) - 65) `mod` 26) + 65) :
          go codeword (n + 1) xs
      | otherwise =
          x :
          go codeword n xs

main :: IO ()
main = do
  putStr "Message to cypher: "
  msg <- getLine
  putStr "Codeword: "
  cw  <- getLine
  putStrLn (caesarV cw msg)

genLetter :: Gen Char
genLetter = elements (' ' : (['a'..'z'] ++ ['A'..'Z']))

genWord :: Gen String
genWord = listOf genLetter

genCodeMsg :: Gen (String, String)
genCodeMsg = (,) <$> (listOf1 (elements (['a'..'z'] ++ ['A'..'Z']))) <*> genWord

propEncodeDecode :: (String, String) -> Bool
propEncodeDecode (xs, ys) = (==) ys $ unCaesarV xs $ caesarV xs ys

testMain :: IO ()
testMain = quickCheck $ forAll genCodeMsg $ propEncodeDecode
