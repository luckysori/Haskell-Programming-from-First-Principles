module Main where

import           Control.Applicative
import           Data.Digits         (digits)
import           Data.Ratio          ((%))
import           Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- integer
  char '/'
  denominator <- integer
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  integral <- integer
  char '.'
  zeros <- try (some $ char '0')
  dec <- decimal
  return
    (fromIntegral integral +
      (fromIntegral dec /
        (fromIntegral (10 ^ ((+) (length $ digits 10 dec)
                                 (length zeros))))))

parseDecimal' :: Parser Double
parseDecimal' = do
  integral <- integer
  char '.'
  zeros <- try (some $ char '0')
  dec <- decimal
  return $ read (show integral ++ "." ++ zeros ++ show dec)

type FractionOrDecimal = Either Rational Double

parseRod :: Parser FractionOrDecimal
parseRod = try (Left <$> parseFraction) <|> (Right <$> parseDecimal)

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
