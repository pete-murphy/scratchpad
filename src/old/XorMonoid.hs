module XorMonoid where

import           Data.Set (Set, intersection, union, (\\))
import qualified Data.Set as S

newtype Xor a =
  Xor
    { getXor :: Set a
    }
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Xor a) where
  Xor xs <> Xor ys = Xor (union xs ys \\ intersection xs ys)

instance Ord a => Monoid (Xor a) where
  mempty = fromList []

fromList :: Ord a => [a] -> Xor a
fromList = Xor . S.fromList

toList :: Xor a -> [a]
toList = S.toList . getXor
