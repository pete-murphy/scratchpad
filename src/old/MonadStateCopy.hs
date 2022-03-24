{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MonadStateCopy where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Text.Printf
import Text.Read (readMaybe)

program :: MonadState Int m => m ()
program = do
  n <- get
  if  | n == 10 -> pure ()
      | even n -> put (n `div` 2 + 1) *> program
      | otherwise -> put (3 * n + 1) *> program

newtype App a = App {runApp :: IORef Int -> IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (IORef Int)
    )
    via ReaderT (IORef Int) IO

type App' = StateT Int IO

type App'' = ReaderT FilePath IO

instance MonadState Int App where
  get :: App Int
  get = do
    stateRef <- ask
    liftIO (readIORef stateRef)
  put :: Int -> App ()
  put n = do
    stateRef <- ask
    liftIO (writeIORef stateRef n)

-- | Bad idea, but just to show another way of implementing: this instance will
-- actually write to a file, and then read from a file as `get` and `put`
-- implementations respectively
instance {-# OVERLAPPING #-} MonadState Int App'' where
  get :: App'' Int
  get = do
    filename <- ask
    liftIO (fromMaybe 0 . readMaybe <$> readFile filename)
  put :: Int -> App'' ()
  put n = do
    filename <- ask
    liftIO (writeFile filename (show n))

main :: IO ()
main = do
  -- App
  stateRef <- newIORef 0
  runApp (program :: App ()) stateRef
  readIORef stateRef
    >>= print
  -- App'
  flip execStateT 0 (program :: App' ())
    >>= print
  -- App'' (warning, bad idea)
  filename <- getLine
  runReaderT (program :: App'' ()) filename
    >>= print
