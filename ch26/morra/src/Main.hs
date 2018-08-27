module Main where

import           Control.Monad             (join)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Console.ANSI
import           System.Environment
import           System.Random             (randomRIO)

data Player = P1 | P2 deriving Eq

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

data PlayerType = Human | CPU
type Mode = (PlayerType, PlayerType)

data GameState = GameState
              { mode         :: Mode
              , maxRoundWins :: Int
              , gameScore    :: (Int, Int)
              , roundScore   :: (Int, Int)}

checkRoundWinner :: StateT GameState IO Player
checkRoundWinner = do
  (GameState _ _ (gs1, gs2) (rs1, rs2)) <- get
  let rw = if even $ rs1 + rs2 then P1 else P2
  lift $ putStrLn (show rw ++ " wins round number " ++ show (gs1 + gs2 + 1) ++ ".")
  return rw

updateGameScore :: Player -> StateT GameState IO ()
updateGameScore p = -- p is the player that won the most recent round
  modify (\(GameState m o (s1, s2) rs) -> do
             let ns = if p == P1 then (s1 + 1, s2) else (s1, s2 + 1)
             GameState m o ns rs)

checkGameWinner :: StateT GameState IO (Maybe Player)
checkGameWinner = gets (\(GameState _ o (s1, s2) _)
                        -> if s1 == o then Just P1 else if s2 == o then Just P2 else Nothing)

getNumber :: Int -> StateT GameState IO (Maybe Int)
getNumber n
  | n < 0 || n > 1 = return Nothing -- should never go here
  | otherwise = do
      let fs = [fst, snd]
      (GameState m _ _ _) <- get
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
  modify (\(GameState m o gs _) -> GameState m o gs (p1N, p2N))
  rw <- checkRoundWinner
  updateGameScore rw
  gw <- checkGameWinner
  case gw of
    Just p  -> return p
    Nothing -> morra

readConfig :: [String] -> GameState
readConfig ["2", n] = GameState (Human, Human) (read n) (0,0) (0,0)
readConfig ["1", n] = GameState (Human, CPU) (read n) (0,0) (0,0)
readConfig ["0", n] = GameState (CPU, CPU) (read n) (0,0) (0,0)
readConfig _        = GameState (Human, CPU) 2 (0,0) (0,0)

main :: IO ()
main = do
  gs <- readConfig <$> getArgs
  (winner, _) <- runStateT morra gs
  putStrLn $ show winner ++ " wins the game!"
