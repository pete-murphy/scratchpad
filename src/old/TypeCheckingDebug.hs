module TypeCheckingDebug where

import Control.Monad
import Control.Applicative
import Control.Arrow

foo1 :: Maybe Int
foo1 = pure 5

foo2 :: Int -> Maybe Bool
foo2 n = pure $ n == 5

f :: Maybe (Int, Bool)
f = do
  a <- foo1
  b <- foo2 a
  pure (a, b)

f' :: Maybe (Int, Bool)
f' = foo1 >>= sequenceA . (id &&& foo2)

main = do
  print f
  print f'
-- tjkl = (&&&)
