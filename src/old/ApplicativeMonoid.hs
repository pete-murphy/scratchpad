{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ApplicativeMonoid where

import Control.Applicative

class (Applicative f, Monoid m) => ApplicativeMonoid f m where
  appAppend :: f m -> f m -> f m
  appEmpty  :: f m

instance (Applicative f, Monoid m) => ApplicativeMonoid f m where
  appAppend = liftA2 (<>)
  appEmpty  = pure mempty
 

-- instance (Monoid m) => Monoid (Either e m) where
--   Left x `mappend` _ = Left x
--   _ `mappend` Left y = Left y
--   x `mappend` y      = 

-- instance Semigroup (Either a b) where
--     Left _ <> b = b
--     a      <> _ = a
