{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind
import GHC.Natural (Natural)

class Functor f => Representable f where
  type Rep f :: Type
  tabulate :: (Rep f -> x) -> f x
  index :: f x -> (Rep f -> x)

data Stream a = a :> Stream a
  deriving (Functor)

instance Representable Stream where
  type Rep Stream = Natural
  tabulate f = f 0 :> tabulate (f . (+ 1))

  -- tabulate f = go (0 :: Natural)
  --   where
  --     go n = f n :> go (n + 1)
  index (x :> stream) n
    | n == 0 = x
    | otherwise = index stream (n - 1)
