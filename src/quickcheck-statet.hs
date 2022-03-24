{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Identity (Identity)
import Data.Function (on)
import Data.Tuple (swap)
import Control.Lens (Swapped(swapped))
import Control.Arrow ((&&&), Arrow((***)))

newtype StateT s m a = StateT (s -> m (a, s))
  deriving (Functor)

type State s = StateT s Identity

-- instance Applicative m => Applicative (StateT s m) where
--   pure anything = StateT (\s -> pure (anything, s))
--   StateT stf <*> StateT sta =
--     let newStateFunc s =
--           let mf = stf s
--               ma = sta s
--               g (f, _) (a, s) = (f a, s)
--            in g <$> mf <*> ma
--      in StateT newStateFunc

instance (Applicative k, Monoid s) => Applicative (StateT s k) where
  pure :: a -> StateT s k a
  pure x = StateT \s -> pure (x, s)

  (<*>) ::
    forall s k a b.
    (Applicative k, Monoid s) =>
    StateT s k (a -> b) ->
    StateT s k a ->
    StateT s k b
  -- StateT kf <*> StateT ka =
  --   StateT \s -> do
  --     (f, s') <- kf s
  --     (a, s'') <- ka s'
  --     pure (f a, s'')

  StateT kf <*> StateT ka =
    StateT ((liftA2 . liftA2) hmmm kf ka)
    where
      hmmm :: (a -> b, s) -> (a, s) -> (b, s)
      -- hmmm f a = swap (swap f <*> swap a)
      -- hmmm = _ . (($) *** (<>))
      hmmm = 

-- where
-- hmmm = (<*>) `on` swap
-- hmmm = _df (<*>) swap
--       -- swap (a,b) = (b,a)

put :: Applicative m => s -> StateT s m ()
put s = StateT \_ -> pure ((), s)

get :: Applicative m => StateT s m s
get = StateT \s -> pure (s, s)

modify :: Applicative m => (s -> s) -> StateT s m ()
modify f = StateT \s -> pure ((), f s)

main :: IO ()
main = do
  undefined
