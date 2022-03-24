module StateTListT where

-- if i want to use the List Monad with global state I wrap it in StateT right?
-- Or is it ListT wrapping State? I always get mixed up about the order for
-- transformer stacks

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.List
import Control.Monad.Trans.State

program :: StateT [Int] IO ()
program = do
  modify (map (+ 200))
  n <- get
  liftIO (forM_ n print)

stateOuter :: [()] -> StateT [Int] [] Int
stateOuter zs = do
  (x : xs) <- get
  put xs
  modify (map (+ 1))
  _ <- lift zs
  pure x

main :: IO ()
main = do
  -- evalStateT program [0 .. 20]
  let p = stateOuter (replicate 20 ())
  print $ runStateT p [1, 2]

listOuter :: [()] -> ListT (State [Int]) Int
listOuter ys = do
  (x : xs) <- lift get
  lift (put xs)
  lift (modify (map (+ 100)))
  y <- lift . lift $ 99990
  pure y
