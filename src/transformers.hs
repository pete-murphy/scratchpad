{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor (Bifunctor (first))

newtype StateT s m a
  = StateT
      { runStateT :: s -> m (a, s)
      }

instance (Monad m) => Monad (StateT s m) where
  StateT x >>= f = StateT \s -> do
    (a', s') <- x s
    runStateT (f a') s'

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT \s -> pure (x, s)
  StateT f <*> StateT x = StateT \s -> do
    (ab, s') <- f s
    (a, s'') <- x s'
    pure (ab a, s'')

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT x) = StateT \s ->
    fmap (first f) (x s)

type Parser = StateT String Maybe
