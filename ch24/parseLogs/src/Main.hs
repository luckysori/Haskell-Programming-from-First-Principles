{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.String.Utils   (strip)
import           Data.Time           hiding (parseTime)
import           Text.RawString.QQ
import           Text.Trifecta

type Name = String
type When = (Int, Int)
newtype Duration = Duration (Int, Int) deriving (Eq, Show)

diff :: When -> When -> Duration
diff (x1, y1) (x2, y2) =
  case minDiff < 0 of
    True -> case hourDiff' < 0 of
              True -> error "non-consecutive times"
              _    -> Duration (hourDiff', 60 + minDiff)
              where hourDiff' = x1 - x2 - 1
    _ -> case hourDiff < 0 of
           True -> error "non-consecutive times"
           _    -> Duration (hourDiff, minDiff)
         where hourDiff = x1 - x2
    where minDiff = y1 - y2

data ActivityWhen = ActivityWhen
                { name :: Name
                , time :: When
                }
              deriving Eq

instance Show ActivityWhen where
  show (ActivityWhen n (h, m)) =
    h' ++ ":" ++ m' ++ " " ++ n
    where
      m' = if m < 10 then '0':(show m) else show m
      h' = if h < 10 then '0':(show h) else show h

data ActivityDuration =
  ActivityDuration Name Duration deriving Eq

instance Show ActivityDuration where
  show (ActivityDuration n (Duration (h, m))) =
    h' ++ ":" ++ m' ++ " " ++ n
    where
      m' = if m < 10 then '0':(show m) else show m
      h' = if h < 10 then '0':(show h) else show h

data OneDay =
  OneDay Day [ActivityWhen] deriving Eq

instance Show OneDay where
  show (OneDay d as) = "# " ++ show d ++ "\n" ++ go as
    where
      go []     = "\n"
      go (a:as) = show a ++ "\n" ++ go as

data OneDayDuration =
  OneDayDuration Day [ActivityDuration] deriving (Eq, Show)
data WhenLog =
  WhenLog [OneDay] deriving Eq

instance Show WhenLog where
  show (WhenLog [])     = ""
  show (WhenLog (d:ds)) = show d ++ (show $ WhenLog ds)

data TimeLog =
  TimeLog [OneDayDuration] deriving (Eq, Show)

aLog :: String
aLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

anotherLog :: String
anotherLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
-- checking this kind of comment
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
-- comments here?


# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
-- how about comments here???
|]

parseComment :: Parser String
parseComment = do
  string "--"
  comment <- many $ noneOf "\n"
  (char '\n' >> return ()) <|> eof
  return $ strip comment

-- fromGregorian corrects nonsensical arguments by truncating to min/max
-- would maybe prefer to throw errors
parseDate :: Parser Day
parseDate = do
  char '#'
  some $ char ' '
  xs <- sepBy decimal $ (char '-')
  case length xs of
    3 -> return $ fromGregorian y (fromIntegral m) (fromIntegral d)
         where [y, m, d] = xs
    _ -> fail "incorrect date format"

parseTime :: Parser When
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

parseActivity :: Parser ActivityWhen
parseActivity = do
  start <- parseTime
  many $ char ' '
  name <- manyTill anyChar ((parseComment >> return ()) <|> (string "\n" >> return ()) <|> eof)
  return $ ActivityWhen (strip name) start

parseOneDay :: Parser OneDay
parseOneDay = do
  many $ (string " " <|> string "\n" <|> parseComment)
  date <- parseDate
  many $ (string " " <|> string "\n" <|> parseComment)
  acts <- some $ do
    act <- parseActivity
    many $ parseComment <|> string "\n"
    return act
  return $ OneDay date acts

parseWhenLog :: Parser WhenLog
parseWhenLog = WhenLog <$> some parseOneDay

parseTimeLog :: Parser TimeLog
parseTimeLog = do
  ds <- some parseOneDay
  return $ TimeLog (go ds)
  where
    go [] = []
    go ((OneDay date act):ds) =
      OneDayDuration date (timeSpent act) : go ds
    timeSpent :: [ActivityWhen] -> [ActivityDuration]
    timeSpent [] = []
    timeSpent ((ActivityWhen n t1):as) =
      case as of
        [] -> [ActivityDuration n ((24, 00) `diff` t1)]
        _  ->  ActivityDuration n (time (head as) `diff` t1) :
               timeSpent as

main :: IO ()
main = do
  putStrLn "hello world"
