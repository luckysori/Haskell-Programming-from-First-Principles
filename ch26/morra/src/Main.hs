module Main where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Console.ANSI
import           System.Environment
import           System.Random             (randomRIO)

data Player = P1 | P2

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

data PlayerType = Human | CPU
type Mode = (PlayerType, PlayerType)

data GameState = GameState
              { mode  :: Mode
              , score :: (Int, Int)}

checkWinner :: StateT GameState IO Player
checkWinner = gets (\(GameState _ (h, c))
                    -> if even (h + c) then P1 else P2)

getNumber :: Int -> StateT GameState IO (Maybe Int)
getNumber n
  | n < 0 || n > 1 = return Nothing -- should never go here
  | otherwise = do
      let fs = [fst, snd]
      (GameState m _) <- get
      case fs !! n $ m of
        Human -> do
          lift $ putStrLn $
            "Player " ++
            show (n + 1) ++
            ", please enter a number: "
          lift $ fmap (Just . read) getLine -- can cause exceptions
        CPU   -> lift $ fmap Just (randomRIO (1, 2) :: IO Int)

morra :: StateT GameState IO Player
morra = do
  (Just p1N) <- getNumber 0
  lift clearScreen -- does not work inside emacs
  (Just p2N) <- getNumber 1
  lift $ putStrLn $ "Player 1 chose: " ++ show p1N
  lift $ putStrLn $ "Player 2 chose: " ++ show p2N
  modify (\(GameState m _) -> GameState m (p1N, p2N))
  checkWinner

-- for now it only determines PvP or PvE
readConfig :: [String] -> GameState
readConfig ["2"] = GameState (Human, Human) (0,0)
readConfig ["0"] = GameState (CPU, CPU) (0,0) -- testing
readConfig _     = GameState (Human, CPU) (0,0)

main :: IO ()
main = do
  gs <- readConfig <$> getArgs
  (winner, _) <- runStateT morra gs
  putStrLn $ show winner ++ " wins!"
