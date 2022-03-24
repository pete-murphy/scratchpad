module MapM where

import           Control.Monad

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do
  r <- f x
  rs <- mapM' f xs
  return (r : rs)

mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' _ []     = return []
mapM'' f (x:xs) = (:) <$> f x <*> mapM'' f xs

zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- zipWithM' _ [] _ = return []
-- zipWithM' _ _ [] = return []
-- zipWithM' f (x : xs) (y: ys) =  (:) <$> (f x y) <*> zipWithM' f xs ys
zipWithM' f (xs) (ys) = sequence (zipWith f xs ys)

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma = ma >> sequence (replicate n ma)
