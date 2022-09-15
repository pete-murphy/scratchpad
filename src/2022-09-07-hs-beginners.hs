{-# LANGUAGE RankNTypes #-}

import Data.Coerce

foldMapVia :: (Coercible a b, Monoid b) => (a -> b) -> (x -> a) -> [x] -> a
foldMapVia to f = coerce . foldMap (to . f)

foldMapVia' :: forall b a. (Monoid b, Coercible a b) => (a -> b) -> forall x. (x -> a) -> [x] -> a
foldMapVia' to f = coerce . foldMap (to . f)
