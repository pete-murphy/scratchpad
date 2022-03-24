{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module ReaderT where

import Control.Applicative
import Control.Newtype
import Data.Coerce

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader g) = Reader \r -> f (g r)

instance Applicative (Reader r) where
  pure = Reader . const
  Reader ff <*> Reader g = Reader \r -> ff r (g r)
  liftA2 abc fa fb = Reader \r ->
    let a = coerce fa r
        b = coerce fb r
     in abc a b

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  ma >>= amb = Reader \r ->
    let a = coerce ma r
        f = amb a
     in coerce f r

newtype ReaderT m r a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT m r) where
  fmap f (ReaderT g) = ReaderT \r -> f <$> (g r)

instance Applicative m => Applicative (ReaderT m r) where
  pure = ReaderT . const . pure
  ReaderT ff <*> ReaderT g = ReaderT \r -> ff r <*> g r
  liftA2 abc fa fb = ReaderT \r ->
    let a = coerce fa r
        b = coerce fb r
     in liftA2 abc a b

instance Monad m => Monad (ReaderT m r) where
  (>>=) :: forall a b. ReaderT m r a -> (a -> ReaderT m r b) -> ReaderT m r b
  rma >>= amrb = ReaderT \r -> do
    a <- coerce rma r
    let f = amrb a
    coerce f r

-- ma >>= (flip ($) r) . coerce . amrb