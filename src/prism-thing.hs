{-# LANGUAGE RankNTypes #-}

import Control.Lens

foo :: Prism' s a -> (s -> s) -> a -> Maybe a
foo p f = preview p . f . review p
-- -- foo' :: Prism s t a b -> (s -> t) -> a -> Maybe b
-- -- foo' p f = _waht . f . review p
-- foo' :: Lens' s (Maybe a) -> (s -> s) -> a -> Maybe a
-- foo' l f = set
