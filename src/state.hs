{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import System.Random (StdGen)
import qualified System.Random as Random
import Control.Monad.Trans.State (State(..))
import qualified Control.Monad.Trans.State as State
import Control.Applicative
import Control.Arrow
import Data.Monoid

-- newtype State s a =
--   State { runState :: s -> (a, s) }
-- 
-- instance Functor (State s) where
--   fmap f (State g) = State \s ->
--     let (a, s') = g s
--      in (f a, s')
-- 
-- instance Applicative (State s) where
--   pure a = State \s -> (a, s)
--   State ab <*> State fa = State \s ->
--     let (a, s') = fa s
--         (f, s'') = ab s'
--      in (f a, s')
-- 
-- instance Monad (State s) where
--   State f >>= amb = State \s ->
--     let (a, s') = f s
--         State g = (amb a)
--      in g s'
-- 
-- put :: s -> State s ()
-- put s = State \_ -> ((), s)
-- 
-- get :: State s s
-- get = State \s -> (s, s)
-- 
-- modify :: (s -> s) -> State s ()
-- modify f = State \s -> ((), f s)

-------------------------

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show, Enum)

intToDie :: Int -> Die
intToDie = toEnum . subtract 1

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = Random.mkStdGen 0
      (d1, s1) = Random.randomR (1, 6) s
      (d2, s2) = Random.randomR (1, 6) s1
      (d3, _) = Random.randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = State.state do
  (n, s) <- Random.randomR (1, 6)
  pure (intToDie n, s)

main :: IO ()
main = pure ()


foo :: State.State Int Bool
foo = do 
  -- x <- State.get
  liftA2 h f g <$> State.get
  State.gets (\x -> f x `h` g x)
  State.gets (f <> g) @(via Any)
  uncurry h . (f &&& g) <$> State.get
  x <- State.get
  pure (f x || g x)

  where g = odd
        f =even
        h = (||)