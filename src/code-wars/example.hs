{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}

import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity

newtype Foo a = Foo (Identity (Identity a))
  deriving (Functor, Foldable) -- via (Compose Identity Identity)
  deriving (Traversable)

-- |
-- instance (Monoid a) => Semigroup (Five a) where
--   Five s t u v w <> Five s' t' u' v' w' = Five (s <> s') (t <> t') (u <> u') (v <> v') (w <> w')

-- instance (Monoid a) => Monoid (Five a) where
--   mempty = Five mempty mempty mempty mempty mempty

-- instance Functor Five where
--   fmap f (Five s t u v w) = Five (f s) (f t) (f u) (f v) (f w)

-- instance Applicative Five where
--   pure x = Five x x x x x
--   Five fs ft fu fv fw <*> Five s t u v w = Five (fs s) (ft t) (fu u) (fv v) (fw w)

-- instance Foldable Five where
--   foldMap f (Five s t u v w) = f s <> f t <> f u <> f v <> f w

-- instance Traversable Five where
--   traverse f (Five s t u v w) = Five <$> f s <*> f t <*> f u <*> f v <*> f w
