module Main where

import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
        "intToDie got non 1-6 integer:"
        ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN l g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= l = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged l g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count dieList gen
      | sum >= l = (count, dieList)
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die) (count + 1) ((intToDie die) : dieList) nextGen
             
main :: IO ()
main = undefined
