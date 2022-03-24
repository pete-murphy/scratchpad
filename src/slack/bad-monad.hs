{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative

newtype MaybeTrans m a = MaybeTrans {runMaybeTrans :: m (Maybe a)}
  deriving (Functor)

instance Monad m => Applicative (MaybeTrans m) where
  pure = MaybeTrans . pure . Just
  liftA2 f (MaybeTrans ma) (MaybeTrans mb) = MaybeTrans do
    a <- ma
    b <- mb
    pure (f <$> a <*> b)

instance Monad m => Monad (MaybeTrans m) where
  return = MaybeTrans . return . Just
  m >>= fn = do
    val <- m
    fn val
