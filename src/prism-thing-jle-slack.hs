{-# LANGUAGE StandaloneDeriving #-}

import Data.Set (Set)
import qualified Data.Set as S

data Pair a b = Pair a b

instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair x _ == Pair y _ = x == y

deriving instance (Ord a, Ord b) => Ord (Pair a b)

xs :: Set (Pair Int Int)
xs = S.fromList [Pair 1 1, Pair 2 2, Pair 3 3]

ys :: Set (Pair Int Int)
ys = S.fromList [Pair 3 1, Pair 3 2, Pair 3 3]

f (Pair _ a) = Pair a a

g (Pair a _) = Pair (a + 1) a

main = do
  print (xs == ys) -- False
  print
    (S.map g (S.map f xs) == S.map (g . f) xs) -- True
        -- print
        --   (S.map (g . f) ys == S.map (g . f) xs) -- True
