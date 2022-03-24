{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}

import Control.Applicative
import Data.Foldable
import Data.Functor.Compose

data Five a = Five a a a a a
  deriving (Show)

newtype Card a = Card (Five (Five a))
  deriving (Show, Functor, Foldable, Traversable)

instance (Monoid a) => Semigroup (Five a) where
  Five s t u v w <> Five s' t' u' v' w' = Five (s <> s') (t <> t') (u <> u') (v <> v') (w <> w')

instance (Monoid a) => Monoid (Five a) where
  mempty = Five mempty mempty mempty mempty mempty

instance Functor Five where
  fmap f (Five s t u v w) = Five (f s) (f t) (f u) (f v) (f w)

instance Applicative Five where
  pure x = Five x x x x x
  Five fs ft fu fv fw <*> Five s t u v w = Five (fs s) (ft t) (fu u) (fv v) (fw w)

-- instance Foldable Five where
--   foldMap f tf = fold (fmap f tf)

instance Foldable Five where
  foldMap f (Five s t u v w) = f s <> f t <> f u <> f v <> f w

instance Traversable Five where
  traverse f (Five s t u v w) = Five <$> f s <*> f t <*> f u <*> f v <*> f w

-- >>> import Data.Monoid
-- >>> foldMap Sum (Five 1 2 3 4 5)
-- Sum {getSum = 15}

newtype Matrix a = Matrix [[a]]
  deriving stock (Show, Functor, Foldable, Traversable)
  deriving (Applicative) via (Compose ZipList ZipList)

-- >>> xs = Matrix [[1,2,3],[4,5,6]]
-- >>> sequenceA xs
-- Ambiguous type variables ‘f0’,
--                          ‘a0’ arising from a use of ‘evalPrint’
-- prevents the constraint ‘(Show
--                             (f0 (Matrix a0)))’ from being solved.
-- Probable fix: use a type annotation to specify what ‘f0’,
--                                                     ‘a0’ should be.
-- These potential instances exist:
--   instance [safe] (Show a, Show b) => Show (a :-> b)
--     -- Defined in ‘Test.QuickCheck.Function’
--   instance [safe] (Show a, Show b) => Show (Fun a b)
--     -- Defined in ‘Test.QuickCheck.Function’
--   instance Show (Blind a) -- Defined in ‘Test.QuickCheck.Modifiers’
--   ...plus 152 others
--   (use -fprint-potential-instances to see them all)
