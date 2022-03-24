{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- {-# LANGUAGE ImportQualifiedPost #-}

-- import qualified Control.Monad.State as State
-- import Data.Coerce (coerce)

import Control.Applicative
import Control.Monad.State (State, StateT (StateT), evalState, get, put)
import Data.Functor.Identity (Identity (..))
import Data.Maybe

-- x :: State s a -> (s -> (a, s))
-- x = coerce

-- x' :: (s -> (a, s)) -> State s a
-- x' = coerce

scanMap :: forall a m t. (Semigroup m, Traversable t) => (a -> m) -> t a -> t m
scanMap f xs = evalState (traverse g xs) Nothing
  where
    g :: a -> State (Maybe m) m
    g a = do
      last <- get
      let m = f a
          next = liftA2 (<>) last (Just m)
      put next
      pure (fromMaybe m next)
-- scanMap :: forall a m t. (Monoid m, Traversable t) => (a -> m) -> t a -> t m
-- scanMap f xs = evalState (traverse z xs) mempty
--   where
--     z :: a -> State m m
--     z x = do
--       last <- get
--       let next = last <> f x
--       put next
--       pure next
-- toPoints' :: [Token] -> [Point]
-- toPoints' ts = concat (evalState (traverse f ts) (0, 0))
--   where
--     f :: Token -> State Point [Point]
--     f t = do
--       xs <- gets t
--       put (head xs)
--       pure xs
