module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Text.Trifecta

-- phone numbers are not integers but strings (can have leading zeros)
type NumberingPlanArea = String
type Exchange = String
type LineNumber = String

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseNDigits :: Int -> Parser String
parseNDigits n = replicateM n digit

parseArea :: Parser NumberingPlanArea
parseArea = parseNDigits 3

parseExchange :: Parser Exchange
parseExchange = do
  n <- oneOf ['2'..'9'] <?> "first exchange digit not within 2-9"
  xs <- parseNDigits 2
  return $ n : xs

parseLineNumber :: Parser LineNumber
parseLineNumber = parseNDigits 4

-- re-implementation of skipOptional included in Trifecta
oneOrNone :: Parser a -> Parser (Maybe a)
oneOrNone p = try (Just <$> p) <|> return Nothing

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '(' *> p <* char ')'

-- I hate this function, but I don't know how to make it meaningfully more elegant
parsePhone :: Parser PhoneNumber
parsePhone = do
  dash <- oneOrNone (string "1-")
  case dash of
    Nothing -> do
      bracket <- try (char '(') <|> return ' '
      area <- parseArea
      case bracket of
        '(' -> do
          string ") "
          exchange <- parseExchange
          char '-'
          line <- parseLineNumber
          return $ PhoneNumber area exchange line
        _ -> do
          dash' <- oneOrNone (char '-')
          case dash' of
            Nothing -> do
              exchange <- parseExchange
              line <- parseLineNumber
              return $ PhoneNumber area exchange line
            _ -> do
              exchange <- parseExchange
              char '-'
              line <- parseLineNumber
              return $ PhoneNumber area exchange line
    _ -> do
      area <- parseArea
      char '-'
      exchange <- parseExchange
      char '-'
      line <- parseLineNumber
      return $ PhoneNumber area exchange line

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
