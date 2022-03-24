import Control.Monad (join)

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
  show Nil = ""
  show (Cons x xs) = show x ++ ", " ++ show xs

instance Semigroup (List a) where
  (<>) Nil lst = lst
  (<>) lst Nil = lst
  (<>) l1@(Cons h1 t1) l2 = Cons h1 ((<>) t1 l2)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons fh ft) lst = fmap fh lst <> ((<*>) ft lst)

instance Monad List where
  return x = Cons x Nil

  -- xs >>= k = join $ fmap k xs  -- Option (1)
  (>>=) Nil _ = Nil -- Option (2)
  (>>=) (Cons h t) f = (f h) <> ((>>=) t f) -- Option (2)
