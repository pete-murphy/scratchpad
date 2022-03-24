{-# LANGUAGE DeriveFunctor #-}

module TypeFamilies where

data ExactlyOne a = ExactlyOne a
  deriving (Eq, Ord, Show, Functor)

instance Applicative ExactlyOne where
  pure = ExactlyOne
  ExactlyOne f <*> ExactlyOne a = pure (f a)

u :: ExactlyOne (Int -> Int)
u = undefined

v :: ExactlyOne (Int -> Int)
v = undefined

w :: ExactlyOne Int
w = undefined

foo = pure (.) <*> u <*> v <*> w

foo' = u <*> (v <*> w)
