module Main where

import Data.Word
import Text.Trifecta
import Control.Applicative
import Data.Char (toUpper)
import Numeric (showHex)
import Lib hiding (appendZeros)

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

appendZeros :: String -> String
appendZeros x
  | l < 32 = take (32 - l) (repeat '0') ++ x
  | otherwise = x
  where l = length x

addColons :: String -> String
addColons s = go 1 s
  where
    go _ (x:[]) = [x]
    go n (x:xs)
      | n `mod` 4 == 0 = [x, ':'] ++ go (n + 1) xs
      | otherwise = x : go (n + 1) xs
      
instance Show IPAddress6 where
  show = addColons . appendZeros . flip showHex "" . ipv6ToInt

parseHex :: Parser String
parseHex = do
  xs <- some $ oneOf (['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f'])
  case length xs <= 4 of
    False -> fail "invalid length of hexadecimal string"
    _ -> return xs

-- so so so ugly; need to get back to this exercise and rewrite this parser
-- also doesn't parse ipv4 substrings on the tail of the address
parseIPv6' :: Parser [String]
parseIPv6' = do
  s <- try parseHex <|> return ""
  case s of
    "" -> do  -- ":: hexas" branch (complete?)
      string "::"
      start <- try parseHex <|> (eof >> return "")
      case start of
        "" -> return $ take 8 (repeat "0000")
        _ -> do
          rest <- many $ char ':' >> parseHex
          eof
          let l = 7 - (length rest)
          if l >= 1
            then return $ (take l (repeat "0000")) ++ (start : rest)
            else fail "too many hexadecimal segments after ::"
    _ -> do
      flag <- try
              (string "::" >>= \x -> notFollowedBy (char ':') >> return x) <|>
              (string ":" >>= \x -> notFollowedBy (char ':') >> return x)
      case flag of
        "::" -> do  -- "hexa :: hexas" branch (complete?)
          start <- try parseHex <|> (eof >> return "")
          case start of
            "" -> return $ s : (take 7 (repeat "0000"))
            _ -> do
              rest <- many $ char ':' >> parseHex
              eof
              let l = 6 - (length rest)
              if l >= 1
                then return $ (s : (take l (repeat "0000"))) ++ (start : rest)
                else fail "too many hexadecimal segments after ::"
        _ -> do 
          s' <- sepEndBy1 parseHex (char ':' >> notFollowedBy eof)
          flag' <- try (char ':') <|> (eof >> return ' ')
          let l = length s'
          case flag' of
            ' ' -> do  -- "hexas" branch (complete?)
              if l == 7
                then return $ s : s'
                else fail "wrong number of hexadecimal segments"
            _ ->
              if l > 6
              then fail "wrong number of hexadecimal segments before ::"
              else do
                start <- try parseHex <|> (eof >> return "")
                case start of
                  "" -> return $ (s : s') ++ (take (7 - l) (repeat "0000"))
                  _ -> do
                    rest <- many $ char ':' >> parseHex
                    eof
                    let l' = length rest
                    if (l + l') < 6
                      then return $ (s : s') ++
                           (take (6 - (l + l')) (repeat "0000")) ++ (start : rest)
                      else fail "wrong number of hexadecimal segments around ::"

mapHexToNum :: Char -> Int
mapHexToNum x = (snd . head) $ filter (\y -> fst y == (toUpper x))
                (zip (['0'..'9'] ++ ['A'..'F']) [0..15]) 

hexToDec :: String -> Word64
hexToDec [] = 0
hexToDec (x:xs) = (fromIntegral $ mapHexToNum x) * (16 ^ (length xs)) + hexToDec xs

addZeros :: String -> String
addZeros x
  | l < 4 = take (4 - l) (repeat '0') ++ x
  | otherwise = x
  where l = length x
  
parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  list <- parseIPv6'
  let high = hexToDec . concat . (fmap addZeros) $ take 4 list
      low = hexToDec . concat . (fmap addZeros) $ drop 4 list
  return $ IPAddress6 high low

ipv6ToInt :: IPAddress6 -> Integer
ipv6ToInt (IPAddress6 high low) = (fromIntegral high) * (2^64) + (fromIntegral low)

ipv4ToIPv6 :: IPAddress -> IPAddress6
ipv4ToIPv6 (IPAddress x) = IPAddress6 0 (fromIntegral x)

main :: IO ()
main = do
  let f x = parseString parseIPv6 mempty x
  let g x = parseString parseIPv6 mempty x >>= \y -> return $ ipv6ToInt y
  print $ f "ABCD:EF01:2345:6789:ABCD:EF01:2345:6789"
  print $ f "2001:DB8:0:0:8:800:200C:417A"
  print $ f "2001:DB8::8:800:200C:417A"
  print $ f "2001:0DB8:0000:CD30:0000:0000:0000:0000"
  print $ f "2001:0DB8::CD30:0:0:0:0"
  print $ f "2001:0DB8:0:CD30::"
  print $ f "2001:0DB8::CD30"
  print $ f "2001:0DB8::CD3"
  print $ f "0DB8::CD3"
  print $ f "2001::CD3:0"
  print $ f "2001:ffff::CD3"
  print $ f "::"
  putStrLn $ "Expected 281473568538113:"
  print $ g "0:0:0:0:0:ffff:ac10:fe01"
  putStrLn $ "Expected 281474112159759:"
  print $ g "0:0:0:0:0:ffff:cc78:f"
  putStrLn $ "Expected 42540766411282592856906245548098208122:"
  print $ g "2001:DB8::8:800:200C:417A"
  putStrLn $ "Converting from '192.168.25.234' to IPv6"
  print $ parseString parseIPAddress mempty "192.168.25.234" >>= \x -> return $ ipv4ToIPv6 x
  
