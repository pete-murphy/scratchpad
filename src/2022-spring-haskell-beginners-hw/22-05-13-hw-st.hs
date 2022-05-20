-- sortOn :: Ord c => (a -> c) -> [a] -> [a]
-- sortOn = undefined

data FreeMonad f a
  = Pure a
  | Impure (f (FreeMonad f a))

x :: FreeMonad [] Int
x = Impure [Pure 0, Impure [Pure 1, Pure 2, Impure [Pure 3, Pure 4]]]

-- |
instance Functor f => Functor (FreeMonad f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure x) = Impure ((fmap . fmap) f x)
