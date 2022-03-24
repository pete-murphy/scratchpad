{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative

data Free f a
  = Pure a
  | Impure (f (Free f a))

-- deriving instance (Show (f a)) => Show (Free f a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure x) = Impure (fmap f <$> x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  liftA2 f (Pure a) (Pure b) = Pure (f a b)
  -- f :: a -> b -> c
  -- x :: f (Free f a)
  -- y :: Free f b
  -- _ :: Free f c
  liftA2 f (Impure x) y = Impure (fmap (flip (liftA2 f) y) x)
  liftA2 f x (Impure y) = Impure (fmap (liftA2 f x) y)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= amb = amb a
  -- x   :: f (Free f a)
  -- amb :: a -> Free f b
  -- _   :: Free f b
  Impure x >>= amb = Impure $ fmap (>>= amb) x

data IOF a
  = ReadLine (String -> a)
  | -- | PutStrLn String
    PutStrLn String a

instance Functor IOF where
  -- fmap f (ReadLine g) = ReadLine (\str -> f (g str))
  fmap f (ReadLine g) = ReadLine (f . g)
  fmap f (PutStrLn str a) = PutStrLn str (f a)

type IO' = Free IOF

-- lift = Impure . fmap pure

program :: IO' ()
program = do
  str <- Impure (fmap pure (ReadLine id))
  Impure (fmap pure (PutStrLn str ()))
  Impure (fmap pure (PutStrLn str ()))

nt :: IOF a -> IO a
nt = \case
  ReadLine f -> fmap f getLine
  PutStrLn str x -> putStrLn str *> pure x

-- Way of showing the tree
report :: IOF a -> ([String], a)
report = \case
  ReadLine f -> (["Readline"], f "")
  PutStrLn str x -> (["PutStrLn " <> str], x)

foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree f = \case
  Pure x -> pure x
  Impure x -> foldFree f =<< f x
