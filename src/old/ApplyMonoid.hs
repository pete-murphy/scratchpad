{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ApplyMonoid where

import Data.Functor.Apply (Apply(..))
import Data.Functor.Alt   (Alt(..))

class (Apply f, Monoid m) => ApplyMonoid f m where
  applyAppend :: f m -> f m -> f m
  applyEmpty  :: f m

instance (Monoid m) => ApplyMonoid (Either e) m where
  applyAppend x y = Right (<>) <.> x <.> y
  applyEmpty      = Right mempty
 

-- instance (Monoid m) => Monoid (Either e m) where
--   Left x `mappend` _ = Left x
--   _ `mappend` Left y = Left y
--   x `mappend` y      = 

-- instance Semigroup (Either a b) where
--     Left _ <> b = b
--     a      <> _ = a