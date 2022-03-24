{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import qualified Control.Monad as Monad
import Control.Monad ((>=>))
import qualified Data.Function as Function
import qualified Text.Printf as Printf

data Logger
  = Logger (MVar LogCommand)

data LogCommand
  = Message String
  | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  l <- Logger <$> MVar.newEmptyMVar
  Concurrent.forkIO (logger l)
  pure l

logger :: Logger -> IO ()
logger (Logger m) = Function.fix
  \loop ->
    do
      MVar.takeMVar m >>= \case
        Message msg -> print msg *> loop
        Stop s -> MVar.putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) msg = MVar.putMVar m (Message msg)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- MVar.newEmptyMVar
  MVar.putMVar m (Stop s)
  MVar.takeMVar s

main :: IO ()
main = do
  l <- initLogger
  logMessage l "Hello"
  logMessage l "Bye"
  logStop l
  pure ()
