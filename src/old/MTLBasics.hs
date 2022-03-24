{-# LANGUAGE FlexibleContexts #-}

module MTLBasics where

import Control.Arrow
import Control.Monad.State.Class
import Control.Monad.State.Lazy

-- |
-- >>> countDown 2
-- (0,0)
countDown :: Int -> (Int, Int)
countDown initial = runState program initial

program :: MonadState Int m => m Int
program = do
  n <- get
  if n <= 0
    then pure n
    else put (n - 1) >> program

-- |
-- >>> countDown' 2
-- (0,0)
countDown' :: Int -> (Int, Int)
countDown' = program'

program' :: Int -> (Int, Int)
program' n =
  let n' = get'
   in if n' <= 0
        then return' n'
        else case put' (n' - 1) of
          (_, x) -> program' x
  where
    get' = n
    return' x = (x, x)
    put' x = ((), x)
