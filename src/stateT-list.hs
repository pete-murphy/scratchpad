{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

-- https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html

import Control.Arrow (Arrow (first))

newtype StateT s m a = StateT (s -> m (a, s))

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT k) = StateT \s ->
    fmap (first f) (k s)

instance Applicative m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT \s -> pure (x, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT k <*> StateT sma = StateT \s ->
    let f (ab, _) (a, s') = (ab a, s')
     in f <$> k s <*> sma s

foo :: StateT Int [] Bool
foo = StateT \x -> [(even x, x + 1), (odd x, x - 1), (x > 0, negate x)]
