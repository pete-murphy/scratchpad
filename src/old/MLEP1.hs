{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module MLEP1 where

import Control.Applicative
import Data.Function
import Data.Ord

data Nat' = Zero | Succ Nat'

newtype Nat = Nat {_pred :: forall a. (a -> a) -> a -> a}

instance Show Nat where
  show = show . toInteger'

instance Num Nat where
  Nat n + Nat m = Nat \s z -> n s (m s z)
  Nat n * Nat m = Nat \s z -> n (m s) z
  abs = id
  signum = const one
  fromInteger = \case
    0 -> zero
    n -> one + fromInteger (n - 1)
  Nat n - Nat m = Nat \s z -> n s (m s (z))

one :: Nat
one = Nat ($)

zero :: Nat
zero = Nat (flip const)

toInteger' :: Nat -> Integer
toInteger' (Nat n) = n succ 0
