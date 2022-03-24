{-# LANGUAGE RankNTypes #-}

-- Sam: /djinn (forall r. (a -> r) -> r) -> a
-- Me: were you expecting a success :smile: or just seeing what djinn would do?
-- Sam: I was! I was expecting f g = g id
-- But both
-- Me: I didn’t realize it was possible to implement that…
-- Sam: Ah, it is continuation passing style! We can also have a -> (forall r. (a -> r) -> r)
-- These two types are isomorphic
-- a and (forall r. (a -> r) -> r)

newtype X a = X (forall r. (a -> r) -> r)

newtype Y = Y (forall r. (forall a. a -> r) -> r)
