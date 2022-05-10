{-# LANGUAGE BlockArguments #-}

-- loeb' :: [[a] -> a] -> [a]
-- loeb' fs = xs where xs = fmap (\f -> f xs) fs
-- loeb [length]
--  = xs where xs = fmap (\f -> f xs) [length]
--  = xs where xs = [length xs]
--  = xs where xs = [length [length xs]]
--                           ^^^^^^^^^ this isn't fully evaluated
--  = xs where xs = [length (_:[])]
--                           ^^^^ actually more like this
--  = xs where xs = [1]

-- loeb [sum]
--  = xs where xs = fmap (\f -> f xs) [sum]
--  = xs where xs = [sum xs]
--  = xs where xs = [sum [sum xs]]
--  = xs where xs = [sum [sum [sum xs]]]
--  = xs where xs = ...

-- loeb :: Functor f => f (f a -> a) -> f a
-- loeb fs = xs where xs = fmap (\f -> f xs) fs

import Data.Function (fix)

loeb fs = fix \xs -> fmap (\f -> f xs) fs
