data Foo = Bar | Baz deriving (Eq, Show, Ord)

class Bounded' a where
  minBound' :: a
  maxBound' :: a

instance Bounded' Foo where
  minBound' = Bar
  maxBound' = Baz

instance Bounded' Bool where
  minBound' = False
  maxBound' = True

instance (Bounded' a, Bounded' b) => Bounded' (a, b) where
  minBound' = (minBound', minBound')
  maxBound' = (maxBound', maxBound')

data OneOf a b = This a | That b deriving (Show)

instance (Eq a, Eq b) => Eq (OneOf a b) where
  (==) x y = case x of
    This a -> case y of
      This b -> a == b
      _ -> False
    That a -> case y of
      That b -> a == b
      _ -> False

-- | *** Exercise 1b: ***
-- How about an `Ord` instance for `OneOf`?
-- Again, what assumptions do we need to make? are there other implementations?
instance (Ord a, Ord b) => Ord (OneOf a b) where
  (<=) x y = case x of
    This a -> case y of
      This b -> a <= b
      _ -> False
    That a -> case y of
      That b -> a <= b
      _ -> False

-- | *** Exercise 1c: ***
-- Same for `Bounded'`: assumptions, alternative implementations.
instance (Bounded' a, Bounded' b) => Bounded' (OneOf a b) where
  minBound' = minBound'
  maxBound' = maxBound'

-- | *** Exercise 1d: ***
-- Did you get this far? do you feel good about the instance you just wrote?
-- If so, without typing it in a REPL, can you reason through what this evaluates to?
-- >>> minBound' :: OneOf (OneOf (Bool, Bool), Foo, Foo) (Bool, (Bool, Foo))

{-
_Homework part two: more type classes_

In the last session, we briefly talked about lattices, which could be defined as a type class.
-}
class Lattice a where
  meet :: a -> a -> a
  join :: a -> a -> a

{-
We can visualize what `meet` and `join` mean in the context of a partially-ordered set:
```
    A
  ╱ │ ╲
 ╱  │  ╲
B   C   D
│╲ ╱ ╲ ╱│
│ ╲   ╲ │
│╱ ╲ ╱ ╲│
E   F   G
 ╲  │  ╱
  ╲ │ ╱
    H
```
Supposing we had this as a data type
```
data Eight = A | B | C | D | E | F | G | H
```
we could define a `Lattice` instance for `Eight` such that `join` takes two `Eight`s and returns the "least upper bound" (so `join A B == A`, `join E G == C` and `join D E == A`. Similarly, `meet` gives the "greatest lower bound"—so `meet A B == B`, `meet E G == H` and `meet D E == H`.)
-}

-- | *** Exercise 2a: ***
-- Can we define a `Lattice` instance with the following "instance head"?
instance (Lattice a, Lattice b) => Lattice (a, b) where
  meet (x, y) (x', y') = (meet x x', meet y y')
  join (x, y) (x', y') = (join x x', join y y')
