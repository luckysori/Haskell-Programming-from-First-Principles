-- pg 696

module Phone where

import Data.Char
import Data.List

type Digit = Char
type Presses = Int

-- 1.

data Button = Button Digit String deriving Show
data Phone = Phone [Button] deriving Show

phone :: Phone
phone = Phone
  [ Button '1' []
  , Button '2' "abc"
  , Button '3' "def"
  , Button '4' "ghi"
  , Button '5' "jkl"
  , Button '6' "mno"
  , Button '7' "pqrs"
  , Button '8' "tuv"
  , Button '9' "wxyz"
  , Button '*' ""
  , Button '0' " "
  , Button '#' ".,"
  ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- 2.

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone buttons) char
  | isUpper char = ('*', 1) : go button (toLower char) 0
  | otherwise = go button char 0
  where
    button = filter (\(Button x y) -> elem (toLower char) y || char == x) buttons
    go [] c _ = error ("no button containing char " ++ [c])
    go ((Button digit []):_) _ count = [(digit, count + 1)]
    go ((Button digit (x:xs)):_) c count
      | x == c = [(digit, count + 1)]
      | otherwise = go ((Button digit xs):[]) c (count + 1)

tapsInSentence :: Phone -> String -> [(Digit, Presses)]
tapsInSentence _ [] = []
tapsInSentence phone' (x:xs) = (reverseTaps phone x) ++ (tapsInSentence phone' xs)

-- 3.

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, x) y -> x + y) 0

tapsPerMessage :: [(String, Presses)]
tapsPerMessage = map (\x -> (x, fingerTaps (tapsInSentence phone x))) convo

-- 4.

-- currently case-sensitive
mostPopularLetter :: String -> Char
mostPopularLetter word = go (charFreq word) (' ', 0)
  where
    go [] (p, _) = p
    go (x@(l, n):xs) y@(_, m)
      | not $ isLetter l = go xs y -- only letters are considered 
      | n > m = go xs x
      | otherwise = go xs y

charFreq :: String -> [(Char, Int)]
charFreq word = [ (x, length (filter (==x) word)) | x <- (nub word)]

mostPopularLetterInMessage :: [(String, Char)]
mostPopularLetterInMessage = map (\x -> (x, mostPopularLetter x)) convo

costMostPopularLetter :: Char -> Presses
costMostPopularLetter = fingerTaps . reverseTaps phone

costMostPopularLetterInMessage :: [(String, Char, Presses)]
costMostPopularLetterInMessage =
  map
  (\(x, y) -> (x, y, costMostPopularLetter y))
  mostPopularLetterInMessage

-- 5.

mostPopularLetterInConvo :: Char
mostPopularLetterInConvo = mostPopularLetter $ foldr (++) [] convo

mostPopularWord :: [String] -> String
mostPopularWord sentence = go (wordFreq sentence) ("", 0)
  where
    go [] (p, _) = p
    go (x@(_, n):xs) y@(_, m)
      | n > m = go xs x
      | otherwise = go xs y

wordFreq :: [String] -> [(String, Int)]
wordFreq sentence = [ (x, length (filter (==x) sentence)) | x <- (nub sentence) ]

mostPopularWordInConvo :: String
mostPopularWordInConvo = mostPopularWord (foldr (\x y -> (words x) ++ y) [] convo)
