{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative

data Free f a
  = Pure a
  | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure x) = Impure (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure x = Pure x

  -- Pure f <*> Pure x = Pure (f x)
  -- f <*> Impure g = Impure (fmap (f <*>) g)
  -- Impure f <*> g = Impure (fmap (<*> g) f)
  liftA2 f (Pure a) (Pure b) = Pure (f a b)
  liftA2 f (Impure fa) x =
    Impure do
      let g = fmap @f (fmap @(Free f) f)
          n = g fa
          m = fmap @f (<*> x) n
       in m
  liftA2 f x (Impure fa) =
    Impure do
      let g = fmap @f (fmap @(Free f) (flip f))
          n = g fa
          m = fmap @f (<*> x) n
       in m
-- Pure f <*> Impure x = Impure (fmap (fmap f) x)
-- Impure f <*> Pure x = Impure (fmap (fmap \g -> g x) f)
-- Impure f <*> Impure x = Impure (z :: f (Free f b))
--   where
--     z = undefined

-- instance
