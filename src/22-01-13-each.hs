{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

class Homogeneous t a | t -> a where
  homogeneousToList :: t -> [a]

instance Homogeneous (a, a, a, a) a where
  homogeneousToList (a, a', a'', a''') = [a, a', a'', a''']

main = do
  print (homogeneousToList (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int))
