{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

import Control.Applicative (Applicative (..))
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Prelude hiding (tail)

-- `Free f a` is sum of `a | f (Free f a)`
data Free f a
  = Return a
  | Bind (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Bind ffa) = Bind ((fmap . fmap) f ffa)

instance Functor f => Applicative (Free f) where
  pure = Return
  liftA2 f (Return a) (Return b) = Return (f a b)
  liftA2 f (Bind a) y = Bind do
    let f' = flip f
    liftA2 f' y <$> a
  liftA2 f x (Bind b) = Bind do
    liftA2 f x <$> b

instance Functor f => Monad (Free f) where
  Return a >>= f = f a
  Bind ffa >>= f = Bind do
    (fmap . (=<<)) f ffa

class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  extend f wa = f <$> duplicate wa
  duplicate :: w a -> w (w a)
  duplicate wa = extend id wa
  {-# MINIMAL extract, (duplicate | extend) #-}

-- `Cofree f a` is product of `a & f (Cofree f a)`
data Cofree f a = Cofree a (f (Cofree f a))

instance Functor f => Functor (Cofree f) where
  fmap f (Cofree a fa) = Cofree (f a) ((fmap . fmap) f fa)

instance Functor f => Comonad (Cofree f) where
  extract (Cofree a _) = a
  duplicate (Cofree a fa) = fmap (\a -> Cofree a fa) (Cofree a fa)

-- | Returns the label for a tree.
head :: forall f a. Cofree f a -> a
head (Cofree a _) = a

-- | Returns the "subtrees" of a tree.
tail :: forall f a. Cofree f a -> f (Cofree f a)
tail (Cofree _ fa) = fa

explore ::
  forall f g a b.
  (Functor f, Functor g) =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f (a -> b) ->
  Cofree g a ->
  b
explore pair m w =
  case State.runState (runFreeM step m) w of
    (f, cof) -> f (extract cof)
  where
    step :: f (Free f (a -> b)) -> State (Cofree g a) (Free f (a -> b))
    step ff = State.state (pair (fmap (,) ff) . tail)

runFreeM ::
  (f (Free f a) -> m (Free f a)) ->
  (Free f a -> m a)
runFreeM = undefined
-- go
-- where
-- go :: Free f a -> m (Free f a) a
-- go f = case toView f of
--   Return a -> Done <$> pure a
--   Bind g  -> Loop <$> k (i <$> g)
