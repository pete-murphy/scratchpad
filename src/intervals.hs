{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as Map
import Data.Map (Map)

newtype Interval' a = Interval' {runInterval :: (a, a)}

instance Functor Interval' where
  fmap f (Interval' (x, y)) = Interval' (f x, f y)

lower :: Ord a => Interval' a -> a
lower (Interval' (x, y)) = min x y

upper :: Ord a => Interval' a -> a
upper (Interval' (x, y)) = max x y

{---------------------------------------
****************************************
---------------------------------------}

data Interval a
  = Closed a a
  | ClosedOpen a
  | OpenClosed a
  | Open
  | Empty

instance Functor Interval where
  fmap f (Closed lower upper) = Closed (f lower) (f upper)
  fmap f (ClosedOpen lower) = ClosedOpen (f lower)
  fmap f (OpenClosed upper) = OpenClosed (f upper)
  fmap _ Open = Open
  fmap _ Empty = Empty

{---------------------------------------
****************************************
---------------------------------------}

class JoinSemilattice a where
  join :: a -> a -> a

class MeetSemilattice a where
  meet :: a -> a -> a

class (JoinSemilattice a, MeetSemilattice a) => Semilattice a

{---------------------------------------
****************************************
---------------------------------------}

newtype IntervalMap k a = IntervalMap (Map (Interval k) a)

type EventKey = String

type HiLo = (Int, Int) -- min and max y values

isInInterval :: Int -> Interval Int -> Bool
isInInterval n = \case
  Closed l u -> n >= l && n <= u
  ClosedOpen l -> n >= l
  OpenClosed u -> n <= u
  Open -> True
  Empty -> False

insertInterval ::
  IntervalMap Int (EventKey, HiLo) ->
  (Interval Int, (EventKey, HiLo)) ->
  IntervalMap Int (EventKey, HiLo)
insertInterval (IntervalMap m) (interval, (x, y)) =
  let overlappingIntervals = case interval of
        Closed l u -> undefined
        ClosedOpen l -> undefined
        OpenClosed u -> undefined
        Open -> undefined
        Empty -> undefined
   in undefined
-- stack ::
--   IntervalMap Int (Key, (Int, Int)) ->
--   IntervalMap Int (Key, (Int, Int)) ->
--   IntervalMap Int (Key, (Int, Int))
-- stack (IntervalMap m) (IntervalMap m') = do
--   -- Check to see if any Interval in m' overlaps with any interval in m
--   let intervalsFromLeft = Map.keys m
--       timestampsFromRight = Map.keys m' >>= extractKeys
--       extractKeys = \case
--         Closed l u -> [l, u]
--         ClosedOpen l -> [l]
--         OpenClosed u -> [u]
--         Open -> []
--         Empty -> []
--   -- For every
--   undefined
