module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Test.Hspec
import System.IO.Silently

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO(0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char] deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (\x -> const Nothing x) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ l) c = elem c l

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length [ x | x <- guessed, not $ elem x wordToGuess ] > 7) then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must bet a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

testMain :: IO ()
testMain = hspec $ do
  describe "fillInCharacter" $ do
    it "returns \
       \(Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['a','o','r'])\
       \ for (Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ 'a'"
      $ do
      (fillInCharacter (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r']) 'a') `shouldBe` (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['a','o','r'])
    it "returns \
       \(Puzzle \"word\" [Just 'w', Just 'o', Just 'r', Nothing] ['w','o','r'])\
       \ for (Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ 'w'"
      $ do
      (fillInCharacter (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r']) 'w') `shouldBe` (Puzzle "word" [Just 'w', Just 'o', Just 'r', Nothing] ['w','o','r'])
  describe "handleGuess" $ do
    it "returns \
       \(Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ for (Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ 'r'"
      $ do
      result <- silence $ handleGuess (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r']) 'r'
      result `shouldBe` (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])
    it "returns \
       \(Puzzle \"word\" [Just 'w', Just 'o', Just 'r', Nothing] ['w','o','r'])\
       \ for (Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ 'w'"
      $ do
      result <- silence $ handleGuess (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r']) 'w'
      result `shouldBe` (Puzzle "word" [Just 'w', Just 'o', Just 'r', Nothing] ['w','o','r'])
    it "returns \
       \(Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['a','o','r'])\
       \ for (Puzzle \"word\" [Nothing, Just 'o', Just 'r', Nothing] ['o','r'])\
       \ 'a'"
      $ do
      result <- silence $ handleGuess (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['o','r']) 'a'
      result `shouldBe` (Puzzle "word" [Nothing, Just 'o', Just 'r', Nothing] ['a','o','r'])
