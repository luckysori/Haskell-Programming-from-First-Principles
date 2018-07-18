module Main where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.Time hiding (parseTime)
import Text.Parsec.Combinator (lookAhead)

type Name = String
type TimeSpent = (Int, Int)
data Activity = Activity Name TimeSpent

data OneDay = OneDay Day [Activity]
data Log = Log [OneDay]

parseComment :: Parser String
parseComment = do
  string "--"
  comment <- many $ noneOf "\n"
  char '\n'
  return comment

-- fromGregorian corrects nonsensical arguments by truncating to min/max
-- would maybe prefer to throw errors
parseDate :: Parser Day
parseDate = do
  char '#'
  some $ noneOf ['0'..'9']
  xs <- sepBy integer $ char '-'
  case length xs of
    3 -> return $ fromGregorian y (fromIntegral m) (fromIntegral d)
         where [y, m, d] = xs
    _ -> fail "incorrect date format"

parseTime :: Parser (Int, Int)
parseTime = do
  hour <- integer
  if hour < 0 || hour > 23
    then error "invalid hour"
    else do
    char ':'
    mins <- integer
    if mins < 0 || mins > 59
      then error "invalid minute"
        else return (fromIntegral hour, fromIntegral mins)

-- unfinished: need to figure out how to look ahead. Maybe using lookAhead ;)
parseActivity :: Parser Activity
parseActivity = do
  start <- parseTime
  many $ char ' '
  name <- some $ noneOf "\n"
  return $ Activity name start -- this is super wrong

parseOneDay :: Parser OneDay
parseOneDay = undefined
  
main :: IO ()
main = do
  putStrLn "hello world"
