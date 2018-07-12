import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo f l =
  execState (mapM_ addResult [l,(l-1)..f]) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = do
  mapM_ putStrLn fbft100
  putStrLn $ show ((==) fbft100 fbl100)
  where fbft100 = fizzbuzzFromTo 1 100
        fbl100 =  (reverse $ fizzbuzzList [1..100])
