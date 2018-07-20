module Main where

import Data.Word
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

parseWord8 :: Parser Word8
parseWord8 = do
  n <- integer
  if n >= 0 && n < 256
    then return (fromIntegral n)
    else fail "number of out range 0-255"
    
toBinary :: Word8 -> String
toBinary 0 = show 0
toBinary n = toBinary (n `quot` 2) ++ (show $ n `rem` 2)

lstripZeros :: String -> String
lstripZeros [] = []
lstripZeros ('0':xs) = lstripZeros xs
lstripZeros s = s

to8bits :: String -> String
to8bits s
  | lengthS > 8 = lstripZeros s
  | lengthS < 8 = (take (8 - lengthS) (repeat '0')) ++ s
  | otherwise = s
  where lengthS = length s

toDec :: String -> Word32
toDec [] = 0
toDec (x:xs) = (read [x]) * (2 ^ (length xs)) + toDec xs

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  ns <- sepBy parseWord8 (char '.')
  case length ns of
    4 -> return $ IPAddress ((toDec . concat . (fmap to8bits) . (fmap toBinary)) ns)
    _ -> fail "incorrect format for IPv4"

main :: IO ()
main = do
  putStrLn "The IPv4 address 172.16.254.1 has decimal representation:"
  print $ parseString parseIPAddress mempty "172.16.254.1"
  putStrLn "The IPv4 address 204.120.0.15 has decimal representation:"
  print $ parseString parseIPAddress mempty "204.120.0.15"
