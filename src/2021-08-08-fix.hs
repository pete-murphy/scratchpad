{-# LANGUAGE NoImplicitPrelude #-}

import Data.Bifunctor (Bifunctor (..))
import Data.Monoid (Ap (..))
import Prelude hiding (fold, map)

newtype Fix s a = In {out :: s a (Fix s a)}

map :: Bifunctor s => (a -> b) -> Fix s a -> Fix s b
map f = In . bimap f (map f) . out

instance Bifunctor s => Functor (Fix s) where
  fmap = map

fold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
fold f = f . second (fold f) . out

-- |
-- >>> import Data.Monoid
-- >>> pure (+) <*> Ap [1,2] <*> Ap [2, 4]
-- Ap {getAp = [3,5,4,6]}

-- >>> pure (+) <*> [1,2] <*> [2,4]
-- [3,5,4,6]
