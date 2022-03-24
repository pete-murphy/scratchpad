{-# LANGUAGE BlockArguments #-}

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import System.IO as IO
import qualified Text.Printf as Printf

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin NoBuffering
  m <- MVar.newEmptyMVar
  -- MVar.putMVar m '('
  -- Monad.replicateM_
  --   10
  --   (MVar.putMVar m '*')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 200 *> MVar.putMVar m '|')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 90 *> MVar.putMVar m '-')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 20 *> MVar.putMVar m ')')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 25 *> MVar.putMVar m '(')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 80 *> MVar.putMVar m '=')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 70 *> MVar.putMVar m '/')
  Concurrent.forkIO do
    Monad.forever
      (Concurrent.threadDelay 110 *> MVar.putMVar m '\\')
  Concurrent.forkIO do
    Concurrent.threadDelay 5000000 *> MVar.putMVar m '\r'
  Function.fix \loop -> do
    r <- MVar.takeMVar m
    Monad.unless
      (r == '\r')
      (putChar r *> loop)
  -- Monad.replicateM_ 2 do
  -- r <- MVar.takeMVar m
  -- print r
  pure ()
