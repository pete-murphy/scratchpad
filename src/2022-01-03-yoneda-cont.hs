{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

import GHC.IO (unsafePerformIO)
import System.Mem
import System.Random

newtype Yoneda f a = Yoneda (forall r. (a -> r) -> f r)

instance Functor (Yoneda f) where
  fmap f (Yoneda ya) = Yoneda \g -> ya (g . f)

newtype Cont a = Cont (forall r. (a -> r) -> r)

f :: (forall r. (a -> r) -> r) -> a
f g = g id

f' :: (forall r. (a -> r) -> () -> r) -> () -> a
f' g = g id
-- f' :: Functor f => (forall r. (a -> r) -> f r) -> f a
-- f' g = g id

-- data Foo = Foo Int
--   deriving (Show)

-- main :: IO ()
-- main = do
--   let foo _ = unsafePerformIO (randomIO :: IO Int)
--   print (id (foo ()))
--   print (id (foo ()))
