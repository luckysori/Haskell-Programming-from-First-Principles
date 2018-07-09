-- pg 814

-- 4.

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =
  NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person

mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

printPossiblePerson :: Either PersonInvalid Person -> IO ()
printPossiblePerson (Right person) =
  putStrLn $ show person
printPossiblePerson (Left NameEmpty) =
  putStrLn $ "Error occurred: " ++ show NameEmpty
printPossiblePerson (Left AgeTooLow) =
  putStrLn $ "Error occurred: " ++ show AgeTooLow
printPossiblePerson (Left (PersonInvalidUnknown s)) =
  putStrLn $ "Error occurred: " ++ show (PersonInvalidUnknown s)

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn "Please enter your age: "
  age <- readLn
  let person = mkPerson name (age :: Integer)
  printPossiblePerson person
