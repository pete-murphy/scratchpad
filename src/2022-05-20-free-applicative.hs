{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant pure" #-}

import Control.Monad

data FreeApplicative f a where
  Pure :: a -> FreeApplicative f a
  Apply :: FreeApplicative f (a -> b) -> f a -> FreeApplicative f b

instance Functor (FreeApplicative f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Apply x fc) = Apply (fmap (fmap f) x) fc

instance Applicative (FreeApplicative f) where
  pure = Pure
  fab <*> fa = case fa of
    Pure a -> ($ a) <$> fab
    Apply fca fc -> Apply ((.) <$> fab <*> fca) fc

data HTTPReq a where
  Get :: String -> HTTPReq String

get :: String -> FreeApplicative HTTPReq String
get endpoint = Apply (Pure id) (Get endpoint)

eval :: FreeApplicative HTTPReq a -> IO a
eval (Pure a) = pure a
eval (Apply f (Get endpoint)) = do
  putStrLn ("calling: " <> endpoint)
  eval (f <*> pure "foo")

program :: FreeApplicative HTTPReq ()
program = do
  foo <- get "/api/1"
  bar <- get "/api/foo"
  replicateM_ 5 (get "whatever")
  pure ()

{- There's a parallel to SnocList concatenation -}
data SnocList a = Nil | Snoc (SnocList a) a

concatSL :: SnocList a -> SnocList a -> SnocList a
concatSL sl1 Nil = sl1
concatSL sl1 (Snoc xs x) = Snoc (concatSL sl1 xs) x
