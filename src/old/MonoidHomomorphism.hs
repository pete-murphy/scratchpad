module MonoidHomomorphism where

import Prelude ((++))

class Monoid m where
  mappend :: m -> m -> m
  mempty :: m

instance Monoid [a] where
  mappend = (++)
  mempty = []
