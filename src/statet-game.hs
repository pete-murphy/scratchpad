{-# LANGUAGE BlockArguments #-}

import Control.Monad.State (StateT (..), get, gets, lift)
import System.Random (Random (randomRIO))

data GameState
  = GameState
      { score :: Int,
        turns :: Int
      }

main' :: StateT GameState IO ()
main' = do
  score' <- gets score
  n <- lift (randomRIO (0, 10) :: IO Int)
  guess <- lift getLine
  if read guess == n then 
  lift do
    if (score' > 10)
      then print "You lose!"
      else print "Congrats 🎉🎉🎉! You won!"
