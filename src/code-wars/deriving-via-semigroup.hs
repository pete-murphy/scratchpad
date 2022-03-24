{-# LANGUAGE DerivingVia #-}

import Control.Applicative
import Data.Bifunctor.Join
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Product

-- data Foo a = Foo a a
--   deriving (Semigroup) via (Join Product)

-- data Tree a
--   = Leaf a
--   | Branch (Pair (Tree a))

type Tree = AST Pair Int

data Pair a
  = Pair a a

-----------------

type Math = AST Operator Int

data Operator a
  = Add a a
  | Sub a a
  | Mul a a
  | Neg a

---

data AST f a
  = Leaf a
  | Branch (f (AST f a))

-- | -------
-- test :: [Int] -> Int

-- test = (*) <$> sum <*> product
-- test = do
--   s <- sum
--   p <- product
--   pure (s * p)
test xs =
  ( do
      s <- sum
      p <- product
      pure (s * p)
  )
    xs

-- test xs = do
--   let s = sum xs
--       p = product xs
--   s * p

-- test = do
--   s <- sum
--   p <- product
--   pure (s * p)

-- test =
--   sum >>= \s -> product >>= \p -> pure (s * p)

-- -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
-- ra >>= arb = \r ->
--   let a = ra r
--       b = arb a r
--    in b

-- test =
--   \r -> sum >>= \s -> product >>= \p -> pure (s * p)
-- (>>=) :: [Int] -> Int -> (Int -> [Int] -> Int) -> [Int] -> Int

-- f :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- f = liftA2 (&&)


