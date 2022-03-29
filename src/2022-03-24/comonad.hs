{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe

class Functor f => Comonad f where
  extract :: f a -> a
  duplicate :: f a -> f (f a)
  duplicate fa = extend id fa
  extend :: (f a -> b) -> f a -> f b
  extend f fa = f <$> duplicate fa
  {-# MINIMAL extract, (duplicate | extend) #-}

instance Comonad NonEmpty where
  extract = NonEmpty.head
  duplicate fa@(_ :| rest) = fa :| Maybe.mapMaybe NonEmpty.nonEmpty (List.tails rest)

xs :: NonEmpty Int
xs = NonEmpty.fromList [1, 2, 3, 4]

--
-- Store
--

data Store s a = Store s (s -> a)
  deriving (Functor)

instance Comonad (Store s) where
  extract (Store s f) = f s
  duplicate (Store s f) = Store s \s' -> Store s' f
  extend g store@(Store s f) = Store s \s' -> g (Store s' f)

experiment ::
  Functor f =>
  (s -> f s) ->
  Store s a ->
  f a
experiment sfs (Store s sa) = sa <$> sfs s

--
-- Env
--

data Env e a = Env e a
  deriving (Functor)

instance Comonad (Env e) where
  extract (Env _ a) = a
  duplicate (Env e a) = Env e (Env e a)
  extend f env@(Env e a) = Env e (f env)
