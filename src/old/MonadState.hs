{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

module MonadState where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Text.Printf

program :: MonadState Int m => m ()
program = do
  n <- get
  if  | n == 10 -> pure ()
      | even n -> put (n `div` 2 + 1) *> program
      | otherwise -> put (3 * n + 1) *> program

test :: IO ()
test =
  let initialState = 0
      expectedResult = 10
      actualResult = execState (program :: State Int ()) initialState
   in if actualResult == expectedResult
        then putStrLn "Test passed!"
        else error do
          printf
            "Test failed: expectedResult = %s, actualResult = %s"
            (show expectedResult)
            (show actualResult)

newtype App a = App {runApp :: IORef Int -> IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (IORef Int)
    )
    via ReaderT (IORef Int) IO

newtype App' a = App' {runApp' :: StateT Int IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )
    via StateT Int IO

instance MonadState Int App where
  get :: App Int
  get = do
    stateRef <- ask
    liftIO (readIORef stateRef)
  put :: Int -> App ()
  put n = do
    stateRef <- ask
    liftIO (atomicWriteIORef stateRef n)

instance MonadState Int App' where
  get :: App' Int
  get = get
  put :: Int -> App' ()
  put n = put n

main :: IO ()
main = do
  stateRef <- newIORef 0
  runApp (program :: App ()) stateRef
  readIORef stateRef
    >>= print
  flip execStateT 0 (runApp' (program :: App' ()))
    >>= print
