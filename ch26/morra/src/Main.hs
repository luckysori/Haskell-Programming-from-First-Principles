module Main where

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

type Parity = Int -> Bool

data GameState = GameState
              { mode         :: Mode
              , p1WinsOn     :: Parity
              , maxRoundWins :: Int
              , gameScore    :: (Int, Int)
              , roundScore   :: (Int, Int)}

checkRoundWinner :: StateT GameState IO Player
checkRoundWinner = do
  (GameState _ f _ (gs1, gs2) (rs1, rs2)) <- get
  let rw = if f $ rs1 + rs2 then P1 else P2
  lift $ putStrLn (show rw ++ " wins round number " ++ show (gs1 + gs2 + 1) ++ ".")
  return rw

updateGameScore :: Player -> StateT GameState IO ()
updateGameScore p = -- p is the player that won the most recent round
  modify (\(GameState m f o (s1, s2) rs) -> do
             let ns = if p == P1 then (s1 + 1, s2) else (s1, s2 + 1)
             GameState m f o ns rs)

checkGameWinner :: StateT GameState IO (Maybe Player)
checkGameWinner = gets (\(GameState _ _ o (s1, s2) _)
                        -> if s1 == o then Just P1 else if s2 == o then Just P2 else Nothing)

getNumber :: Int -> StateT GameState IO (Maybe Int)
getNumber n
  | n < 0 || n > 1 = return Nothing -- should never go here
  | otherwise = do
      let fs = [fst, snd]
      (GameState m _ _ _ _) <- get
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
  modify (\(GameState m f o gs _) -> GameState m f o gs (p1N, p2N))
  rw <- checkRoundWinner
  updateGameScore rw
  (GameState _ _ _ (gs1, gs2) _) <- get
  lift $ putStrLn $ "The score is " ++ show gs1 ++ "-" ++ show gs2
  gw <- checkGameWinner
  case gw of
    Just p  -> return p
    Nothing -> morra

selParFunc :: Int -> Parity
selParFunc n = if even n then even else odd

readConfig :: [String] -> GameState
readConfig ["2", p, rw] = GameState (Human, Human) (selParFunc $ read p) (read rw) (0,0) (0,0)
readConfig ["1", p, rw] = GameState (Human, CPU) (selParFunc $ read p) (read rw) (0,0) (0,0)
readConfig ["0", p, rw] = GameState (CPU, CPU) (selParFunc $ read p) (read rw) (0,0) (0,0)
readConfig _            = GameState (Human, CPU) even 2 (0,0) (0,0)
-- default: human vs cpu, human wins rounds on even sums, game won on first to 2 rounds

main :: IO ()
main = do
  gs <- readConfig <$> getArgs
  (winner, _) <- runStateT morra gs
  putStrLn $ show winner ++ " wins the game!"
