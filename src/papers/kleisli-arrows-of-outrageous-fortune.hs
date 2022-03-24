{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

import Data.Kind

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat

data Vec :: Type -> Nat -> Type where
  Nil :: Vec a 'Z
  Cons :: a -> Vec a n -> Vec a ('S n)
