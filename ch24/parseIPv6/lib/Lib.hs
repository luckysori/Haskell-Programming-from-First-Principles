module Lib where

import Data.Word
import Text.Trifecta
import Numeric (showIntAtBase)
import Data.List.Split (chunksOf)
import Data.List (intersperse)

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

toBin :: (Show a, Integral a) => a -> String
toBin x = showIntAtBase 2 (\n -> ['0'..'1']!!n) x ""

toDec' :: (Read a, Num a) => String -> a
toDec' [] = 0
toDec' (x:xs) = (read [x]) * (2 ^ (length xs)) + toDec' xs

appendZeros :: String -> String
appendZeros x
  | l < 32 = take (32 - l) (repeat '0') ++ x
  | otherwise = x
  where l = length x

instance Show IPAddress where
  show (IPAddress x) =
    (concat .
     (intersperse ".") .
     (fmap show) .
     (fmap toDec') .
     (chunksOf 8) .
     appendZeros .
     toBin)
    x

parseWord8 :: Parser Word8
parseWord8 = do
  n <- integer
  if n >= 0 && n < 256
    then return (fromIntegral n)
    else fail "number of out range 0-255"

toBinary :: Word8 -> String
toBinary = toBin

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
toDec = toDec'

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  ns <- sepBy parseWord8 (char '.')
  case length ns of
    4 -> return $ IPAddress ((toDec . concat . (fmap to8bits) . (fmap toBinary)) ns)
    _ -> fail "incorrect format for IPv4"

ipv4ToInt :: IPAddress -> Word32
ipv4ToInt (IPAddress x) = x
