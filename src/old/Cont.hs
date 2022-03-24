{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cont where

import Control.Applicative
import Control.Monad

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont arr) = Cont \br -> arr (br . f)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont \f -> f x
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  Cont abrr <*> Cont arr = Cont \br -> abrr (arr . (.) br)

instance Monad (Cont r) where
  (>>=) :: forall a b. Cont r a -> (a -> Cont r b) -> Cont r b
  Cont arr >>= f = Cont
    \br ->
      let ar :: a -> r
          ar = (\brr -> brr br) . runCont . f
       in arr ar

addC :: Num a => a -> a -> Cont r a
addC x y = Cont \f -> f (x + y)

mulC :: Num a => a -> a -> Cont r a
mulC x y = Cont \f -> f (x * y)

twenty :: Int
twenty = flip runCont id do
  two <- addC 1 1
  five <- addC two 3
  ten <- mulC two five
  mulC ten ten

list :: [Int]
list = flip runCont (: []) do
  as <- Cont \f -> f 1 <> f 2 <> f 3
  bs <- Cont \f -> f 8 <> f 9 <> f as
  pure bs

newtype ContT r m a = ContT {runContT :: (a -> r) -> m r}

newtype Foo x y a = Foo {runFoo :: x -> y -> a}

instance Functor (Foo x y) where
  fmap f (Foo xya) = Foo \x y -> f (xya x y)

instance Applicative (Foo x y) where
  pure a = Foo \_ _ -> a
  Foo f <*> Foo xya = Foo \x y -> f x y (xya x y)

instance Monad (Foo x y) where
  Foo xya >>= f = Foo \x y -> runFoo (f $ xya x y) x y

newtype FooT x y m a = FooT {runFooT :: x -> y -> m a}

instance Monad m => Functor (FooT x y m) where
  fmap f (FooT xyma) = FooT \x y -> f <$> xyma x y

instance Monad m => Applicative (FooT x y m) where
  pure a = FooT \_ _ -> pure a
  FooT f <*> FooT xyma = FooT \x y -> f x y <*> xyma x y

instance Monad m => Monad (FooT x y m) where
  FooT xyma >>= f = FooT \x y -> xyma x y >>= \a -> runFooT (f a) x y

newtype FooT' x y m a = FooT' {runFooT' :: x -> m (y -> a)}

instance Monad m => Functor (FooT' x y m) where
  fmap f (FooT' xmya) = FooT' \x -> fmap f <$> xmya x

instance Monad m => Applicative (FooT' x y m) where
  pure a = FooT' \_ -> pure \_ -> a
  FooT' f <*> FooT' xmya = FooT' \x -> do
    yab <- f x
    ya <- xmya x
    pure \y -> yab y (ya y)

instance Monad m => Monad (FooT' x y m) where
  FooT' xmya >>= f = FooT' \x -> do
    ya <- xmya x
    pure (error "Can't implement")
