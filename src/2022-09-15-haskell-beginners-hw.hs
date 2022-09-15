{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

import Data.Coerce (Coercible, coerce)

pick ::
  (Coercible (Maybe a) b, Semigroup b) =>
  (Maybe a -> b) ->
  [a] ->
  Maybe a
pick to xs =
  if null xs
    then Nothing
    else coerce $ foldl1 (<>) $ to . Just <$> xs

newtype Min a = Min a
  deriving (Show)

instance Ord a => Semigroup (Min a) where
  (<>) :: Ord a => Min a -> Min a -> Min a
  Min x <> Min y = if x < y then Min x else Min y

-- 2. Why can’t we define a Second newtype such that
--   pick Second [1,2,3] -- returns Just 2

-- A: we would need an index, or a way to keep track of how many times the function was called.

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show)

example :: Tree Int
example =
  Branch
    5
    ( Branch
        3
        ( Branch
            2
            (Branch 1 Leaf Leaf)
            Leaf
        )
        (Branch 4 Leaf Leaf)
    )
    ( Branch
        7
        (Branch 6 Leaf Leaf)
        (Branch 8 Leaf Leaf)
    )

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f tree = case tree of
    Leaf -> mempty
    Branch a tr tr' -> foldMap f tr <> f a <> foldMap f tr'

newtype PreOrderTree a = PreOrderTree (Tree a)
  deriving (Show)

instance Foldable PreOrderTree where
  foldMap :: Monoid m => (a -> m) -> PreOrderTree a -> m
  foldMap f tree = case tree of
    PreOrderTree Leaf -> mempty
    PreOrderTree (Branch a tr tr') ->
      f a <> foldMap f (PreOrderTree tr) <> foldMap f (PreOrderTree tr')
-- >>> foldMap (\x -> [x]) (PreOrderTree example)
-- [5,3,2,1,4,7,6,8]

--  == [5,3,2,1,4,7,6,8]
-- False
