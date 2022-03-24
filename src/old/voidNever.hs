{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Data.Void
import Prelude hiding (Maybe (..))

type Maybe a = Either Void a

type Maybe' a = forall b. Either b a

getOrElse :: a -> Maybe a -> a
getOrElse x m = case m of
  Left _ -> x
  Right y -> y

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c -2, r -1),
      (c -2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c -1, r -2),
      (c -1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')
