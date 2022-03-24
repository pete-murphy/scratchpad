{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Limit where

import           Control.Applicative (liftA2)
import           Numeric.Natural     (Natural)

data Limit' a
  = LimitTo' a
  | Unlimited'

data Limit (a :: *) where
  LimitTo :: a -> Limit a
  Unlimited :: Limit a

limitedBy :: Limit Natural -> Integer -> Bool
limitedBy (LimitTo n) m = m == fromIntegral n
limitedBy _ _           = False

instance Functor Limit where
  fmap f (LimitTo n) = LimitTo (f n)
  fmap _ _           = Unlimited

instance Ord n => Semigroup (Limit n) where
  LimitTo n <> LimitTo n' = LimitTo (min n n')
  x <> Unlimited = x
  Unlimited <> x = x

instance Ord n => Monoid (Limit n) where
  mempty = Unlimited

instance Applicative Limit where
  LimitTo f <*> LimitTo a = LimitTo (f a)
  _ <*> _ = Unlimited
  pure = LimitTo

instance Num n => Num (Limit n) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger n = pure (fromInteger n)
