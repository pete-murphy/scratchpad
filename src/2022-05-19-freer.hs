{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.IORef

-- import Prelude hiding ((>>=))

data Freer f a where
  Pure :: a -> Freer f a
  Bind :: f x -> (x -> Freer f a) -> Freer f a

-- eval :: Freer Maybe a -> Maybe a
-- eval (Pure a) = Just a
-- eval (Bind fx f) = case fx of
--   Nothing -> Nothing
--   Just x -> eval (f x)

data HTTPReq a where
  Get :: String -> HTTPReq String

get :: String -> Freer HTTPReq String
get endpoint = Bind (Get endpoint) pure

eval :: Freer HTTPReq a -> IO a
eval (Pure a) = pure a
eval (Bind (Get endpoint) f) = do
  putStrLn ("calling: " <> endpoint)
  eval (f "foo")

eval' :: IORef Bool -> Freer HTTPReq a -> IO ()
eval' _ (Pure a) = pure ()
eval' ref (Bind (Get endpoint) f) = do
  putStrLn ("calling: " <> endpoint)
  threadDelay 3_000_000
  cancelled <- readIORef ref
  if cancelled then putStrLn "cancelled" else eval' ref (f "foo")

main :: IO ()
main = do
  ref <- newIORef False
  _ <- forkIO (eval' ref program)
  getChar >>= \case
    'x' -> writeIORef ref True
    _ -> pure ()

program :: Freer HTTPReq ()
program = do
  foo <- get "/api/1"
  bar <- get ("/api/" <> foo)
  replicateM_ 5 (get "whatever")

instance Functor (Freer f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind fx g) = Bind fx (fmap f . g)

instance Monad (Freer f) where
  Pure a >>= f = f a
  Bind fx a >>= f =
    Bind fx (\x -> a x >>= f) -- (a >=> f) --

instance Applicative (Freer f) where
  (<*>) = ap
  pure = Pure

-- data List a where
--   Nil :: () -> List a
--   Cons :: a -> (a -> List a) -> List a

-- |
-- instance Applicative (Freer f) where
--   fmap f (Pure a) = Pure (f a)
--   fmap f (Bind fx g) = Bind fx (\x -> fmap f (g x))

-- |
-- (>>=) = Bind

-- x :: Freer Maybe ()
-- x =
--   Just 4 >>= \n -> Just "Hello" >>= \str -> Pure ()

-- |
-- data Coyoneda f a where
--   Co :: f x -> (x -> a) -> Coyoneda f a

-- to :: f a -> Coyoneda f a
-- to fa = Co fa id

-- from :: Functor f => Coyoneda f a -> f a
-- from (Co fx xa) = xa <$> fx
