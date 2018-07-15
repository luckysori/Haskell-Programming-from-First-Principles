module Main where

import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type ErrorMsg = String

type Release = [NumberOrString]
type Metadata = [NumberOrString]

parseNumber :: ErrorMsg -> Parser Integer
parseNumber e = do
  n <- some digit
  case head n of
    '0' -> if (length n == 1)
           then return (read n)
           else fail e
    _ -> return (read n) 

parseVersion :: Parser Integer
parseVersion = parseNumber "leading zeros in version number"

parseRelease :: Parser NumberOrString
parseRelease = do
  try (NOSS <$> (some letter)) <|>
    NOSI <$> (parseNumber "leading zeros in release identifier")
    
parseMetadata :: Parser NumberOrString
parseMetadata = try (NOSS <$> (some letter)) <|>
               (NOSI <$> integer)

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  (SemVer ma1 mi1 p1 r1 _) <= (SemVer ma2 mi2 p2 r2 _) =
    case compVersion of
      False -> False
      True -> if ((length r1 > 0) && (length r2 == 0))
              then True
              else if ((length r2 > 0) && (length r1 == 0))
                   then False
                   else compRelease r1 r2
    where
      compVersion = (ma1 <= ma2) && (mi1 <= mi2) && (p1 <= p2)
      compRelease :: Release -> Release -> Bool
      compRelease [] [] = True
      compRelease [] ys = True
      compRelease xs [] = False
      compRelease ((NOSS _) : _) ((NOSI _) : _) = False
      compRelease ((NOSI _) : xs) ((NOSS _) : ys) = compRelease xs ys
      compRelease ((NOSI x) : xs) ((NOSI y) : ys) =
        case compare x y of
          LT -> True
          EQ -> compRelease xs ys
          GT -> False
      compRelease((NOSS x) : xs) ((NOSS y) : ys) =
        case compare x y of
          LT -> True
          EQ -> compRelease xs ys
          GT -> False
          
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- parseVersion
  char '.'
  minor <- parseVersion
  char '.'
  patch <- parseVersion
  release <- try (do
                     char '-'
                     firstR <- parseRelease
                     restR <- many (char '.' >> parseRelease)
                     return $ firstR : restR)
             <|> return []
                 
  metadata <- try (do
                      char '+' 
                      firstM <- parseMetadata
                      restM <- many (char '.' >> parseMetadata)
                      return $ firstM : restM)
              <|> return []
  return $ SemVer major minor patch release metadata
  
main :: IO ()
main = do
  putStrLn $
    "Result of 'parseString parseSemVer mempty \"2.2.1\"':"
  print $ parseString parseSemVer mempty "2.1.1"
  putStrLn $
    "Result of 'parseString parseSemVer mempty \"1.0.0-x.7.z.92\"':"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  putStrLn $
    "Result of 'parseString parseSemVer mempty \"1.0.0-x.7.z.92+alpha.100\"':"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92+alpha.100"
  putStrLn $
    "Result of 'parseString parseSemVer mempty \"1.0.0+alpha.100\"':"
  print $ parseString parseSemVer mempty "1.0.0+alpha.100"
