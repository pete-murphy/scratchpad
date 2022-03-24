{-# LANGUAGE BlockArguments #-}

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

-- {-# LANGUAGE TypeSynonymInstances #-}

import Data.Foldable
import Data.List (intercalate)

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f (State g) = State \s ->
    let (s', a) = g s
     in (s', f a)

instance Applicative (State s) where
  pure x = State \s -> (s, x)
  State fab <*> State fa = State \s ->
    let (s', ab) = fab s
        (s'', a) = fa s'
     in (s'', ab a)

instance Monad (State s) where
  State ma >>= amb = State \s ->
    let (s', a) = ma s
        State f = amb a
     in f s'

get :: State s s
get = State \s -> (s, s)

put :: s -> State s ()
put s = State \_ -> (s, ())

modify :: (s -> s) -> State s ()
modify f = State \s -> (f s, ())

-- Reverse a list, and increase a count of function calls
reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list =
  (funcCount + 1, reverse list)
