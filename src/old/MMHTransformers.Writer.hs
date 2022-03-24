{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Control.Monad (liftM2)

newtype MaybeT m a
  = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mx >>= f = MaybeT do
    mx >>= \case
      Just x -> runMaybeT (f x)
      Nothing -> pure Nothing
  return = MaybeT . return . Just
-- instance (Monad m) => Applicative (MaybeT m) where
--   -- (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
--   -- f <*> x = do
--   --   f' <- f
--   --   f' <$> x
--   liftA2 = liftM2
--   pure = return

-- instance (Monad m) => Functor (MaybeT m) where
--   -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
--   fmap f x = f <$> x

-- data Foo = Foo Int

-- instance Monoid Foo where
--   Foo n `mappend` Foo m = Foo (n + m)

-- instance Semigroup Foo
