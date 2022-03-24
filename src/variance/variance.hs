{-# LANGUAGE InstanceSigs #-}

-- Example of type class declaration
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

class Covariant f where
  comap :: (a -> b) -> (f a -> f b)

instance Covariant [] where
  comap :: (a -> b) -> ([a] -> [b])
  comap aToB xs = map aToB xs

class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)

newtype Equals a = Equals (a -> a -> Bool)

newtype Fn a b = Fn (a -> b)

class Profunctor f where
  promap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'
  lmap :: (a' -> a) -> f a b -> f a' b
  lmap f = promap f id
  rmap :: (b -> b') -> f a b -> f a b'
  rmap f = promap id f

instance Profunctor Fn where
  promap :: (a' -> a) -> (b -> b') -> Fn a b -> Fn a' b'
  promap f g (Fn h) = Fn $ \a' -> g (h (f a'))

instance Contravariant Equals where
  contramap :: (b -> a) -> Equals a -> Equals b
  contramap bToA (Equals comparesA) = Equals $ \b1 -> \b2 ->
    let a1 = bToA b1
        a2 = bToA b2
     in comparesA a1 a2

-- newtype FlipFn a b = Fn (a -> b)

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

newtype Endo a = Endo (a -> a)

newtype SuperEndo a = SuperEndo {unSuperEndo :: (a -> a -> a)}

instance Invariant Endo where
  invmap :: (a -> b) -> (b -> a) -> Endo a -> Endo b
  invmap aToB bToA (Endo aToA) = Endo $ \b -> aToB (aToA (bToA b))

instance Invariant SuperEndo where
  invmap :: (a -> b) -> (b -> a) -> SuperEndo a -> SuperEndo b
  invmap aToB bToA (SuperEndo aToA) = SuperEndo $ \b1 b2 -> aToB (aToA (bToA b1) (bToA b2))

data Color = Red | Blue | Green deriving (Enum, Show)

min' :: SuperEndo Int
min' = SuperEndo $ \x y -> if x < y then x else y

minColor :: SuperEndo Color
minColor = invmap toEnum fromEnum min'
