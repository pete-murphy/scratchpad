{-# LANGUAGE BlockArguments #-}

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Text.Printf as Printf

main :: IO ()
main = do
  m <- MVar.newEmptyMVar
  Concurrent.forkIO (MVar.putMVar m 'x' *> MVar.putMVar m 'y')
  r <- MVar.takeMVar m
  print r
  r <- MVar.takeMVar m
  print r
