{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}

module FreeMonoid where

-- snoc list
data SL a = Empty | SL a :> a

instance Semigroup (SL a) where
  ys <> Empty = ys
  ys <> (xs :> x) = ys <> xs :> x

instance Monoid (SL a) where
  mempty = Empty
  mappend = (<>)

single :: a -> SL a
single x = Empty :> x
-- Monoid homomorphism to list?
-- There must be some `h` such that:
-- h [] = Empty
-- h (xs <> ys) = h xs <> h ys
-- h [x] = single x

-- class FreeMonoid t where
--   singleton :: a -> t a
--   foldMap :: Monoid m => (a -> m) -> t a -> m

-- data FM a where
--   FM :: Monoid m => ((a -> m) -> m) -> FM a

-- unFM :: FM a -> ((a -> m) -> m)
-- unFM (FM f) = f

-- -- instance FreeMonoid FM where
-- singleton :: a -> FM a
-- singleton a = FM (\f -> f a)

-- foldMap = unFM

-- instance Monoid (FM a) where
--   mappend (FM x) (FM y) = FM (\f -> mappend (x f) (y f))
--   mempty = FM (const mempty)
