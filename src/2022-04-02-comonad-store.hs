{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (a -> w b) -> w a -> w b

data Store s a
  = Store s (s -> a)

instance Functor (Store s) where
  fmap f (Store s sa) = Store s (f . sa)

instance Comonad (Store s) where
  extract (Store s sa) = sa s
  duplicate (Store s sa) = Store s \s' -> Store s' sa
  extend f (Store s sa) = f (sa s)
