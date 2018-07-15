module Main where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneEof = one >> eof

oneTwoEof = oneTwo >> eof

comboParser = choice [string "123", string "12", string "1", stop]

-- unlike string, after failing to parse a String the cursor is found before the character which caused the error
string' :: String -> Parser String
string' []         = return []
string' xss@(x:xs) = char x >> string' xs >> return xss

myParseFunc :: Parser Integer
myParseFunc = integer >>= \n -> eof >> return n

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEof:"
  print $ parseString oneEof mempty "123"
  pNL "oneTwoEof:"
  print $ parseString oneTwoEof mempty "123"
  pNL "one, oneTwo or oneTwoThree:"
  print $ parseString comboParser mempty "1"
  print $ parseString comboParser mempty "12"
  print $ parseString comboParser mempty "123"
