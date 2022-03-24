{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)

type App = ReaderT FilePath IO

instance MonadState Bool App where
  get = do
    filename <- ask
    liftIO (read <$> readFile filename)
  put n = do
    filename <- ask
    liftIO (writeFile filename (show n))

isEven :: Int -> Bool
isEven num =
  unsafePerformIO
    do
      flip evalState False do
        put num
        x <- get
        pure x
