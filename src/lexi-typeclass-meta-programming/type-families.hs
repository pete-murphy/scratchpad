{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- Type families are functions from types to types

data Z

data S a

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)

-- | >>> :t Proxy @(Sum (S (S (S Z))) (S (S Z)))
-- | Proxy @(Sum (S (S (S Z))) (S (S Z))) :: Proxy (S (S (S (S (S Z)))))

-- | Or ...
-- | >>> :kind! Sum (S (S (S Z))) (S (S Z))
-- | Sum (S (S (S Z))) (S (S Z)) :: *
-- | = S (S (S (S (S Z))))

-- |
class Flatten a where
  flatten :: a -> [ElementOf a]

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat
-- >>> flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]
-- [1,2,3,4,5,6,7,8]
