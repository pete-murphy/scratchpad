
f :: a -> b -> Int
f = undefined

g :: Int -> d -> e
g = undefined

foo = fmap g . f
foo' = ((.).(.)) 