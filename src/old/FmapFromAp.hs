module FmapFromAp where

fmap' :: Applicative f => (a -> b) -> f a -> f b
fmap' f fa = pure f <*> fa

fmap'' :: Monad m => (a -> b) -> m a -> m b
fmap'' f ma = ma >>= \a -> return (f a)
