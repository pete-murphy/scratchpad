{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Applicative (..))

newtype Compose f g a = Compose {runCompose :: f (g a)}
  deriving (Functor)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap am (Compose fga) = foldMap (foldMap am) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse f (Compose fga) = Compose <$> liftA2 _ _
