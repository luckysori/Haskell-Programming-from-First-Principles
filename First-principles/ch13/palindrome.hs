-- pg 813

-- 2 and 3

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let lineLow = [ toLower x | x <- line1 ]
  let line = [ x | x <- lineLow, elem x ['a'..'z'] ]
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
