{-# LANGUAGE FlexibleContexts #-}

module FreeTalk where

import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Control.Monad.Writer       (MonadWriter)
import           Data.Foldable

newtype Statement =
  PutStrLn String

type Program = [Statement]

helloWorld :: Program
helloWorld = [PutStrLn "Hello", PutStrLn "World"]

manyNumbers :: Program
manyNumbers = execWriter $ for_ [1 .. 5] $ \i -> tell [PutStrLn (show i)]

evalIO :: Program -> IO ()
evalIO = mapM_ go
  where
    go :: Statement -> IO ()
    go (PutStrLn s) = putStrLn s

data Statement'
  = PutStrLn' String
  | GetLine

type Program' = [Statement']
