module Main where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "digit"

base10Integer :: Parser Integer
base10Integer = some (parseDigit <?> "integer") >>= \ns -> return $ read ns

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- try (do char '-')
  some (parseDigit <?> "integer") >>= \ns -> return $ read (sign : ns)
  
main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer' mempty "-123abc"
  
