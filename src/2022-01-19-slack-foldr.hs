{-# LANGUAGE ScopedTypeVariables #-}

map2 :: forall a b. (a -> b) -> [a] -> [b]
map2 fa2b l1 = foldr fn [] l1
  where
    fn :: a -> [b] -> [b]
    fn a b = (:) (fa2b a) b

foo :: Maybe Int
foo =
  (\x y z -> x * y + z)
    <$> pure 1
    <*> pure 2
    <*> pure 3
