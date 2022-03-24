{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Action where

import Prelude

class Monoid m => Action m a where
  act :: m -> a -> a

newtype Self m
  = Self m

instance Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

main :: IO ()
main = pure ()