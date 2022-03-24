{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}

import Control.Applicative
import Data.Coerce
import Data.Functor.Compose

newtype Matrix a = Matrix [[a]]
  deriving stock (Show, Functor, Foldable, Traversable)
  deriving (Applicative) via (Compose ZipList ZipList)

type Matrix' a = ZipList (ZipList a)

matrix :: [[a]] -> ZipList (ZipList a)
matrix = coerce

unMatrix :: Matrix' a -> [[a]]
unMatrix = coerce

-- main :: IO ()
-- main = do
--   let xs = ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
--       m = Matrix xs
--       m' = matrix xs
--   print (unMatrix (sequenceA m'))
--   print (sequenceA m)

-- instance Semigroup s' => Semigroup (s -> s') where
--   f <> g = f . g
append :: (s -> s') -> (s -> s') -> s -> s'
append f g = f . g
