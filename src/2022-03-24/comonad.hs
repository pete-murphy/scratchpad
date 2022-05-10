{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Monoid (Dual (..))

class Functor f => Comonad f where
  extract :: f a -> a
  duplicate :: f a -> f (f a)
  duplicate fa = extend id fa
  extend :: (f a -> b) -> f a -> f b
  extend f fa = f <$> duplicate fa
  {-# MINIMAL extract, (duplicate | extend) #-}

instance Comonad NonEmpty where
  extract = NonEmpty.head
  duplicate fa@(_ :| rest) = fa :| Maybe.mapMaybe NonEmpty.nonEmpty (List.tails rest)

xs :: NonEmpty Int
xs = NonEmpty.fromList [1, 2, 3, 4]

--
-- Store
--

data Store s a = Store s (s -> a)
  deriving (Functor)

instance Comonad (Store s) where
  extract (Store s f) = f s
  duplicate (Store s f) = Store s \s' -> Store s' f
  extend g store@(Store s f) = Store s \s' -> g (Store s' f)

experiment ::
  Functor f =>
  (s -> f s) ->
  Store s a ->
  f a
experiment sfs (Store s sa) = sa <$> sfs s

--
-- Env
--

data Env e a = Env e a
  deriving (Functor)

instance Comonad (Env e) where
  extract (Env _ a) = a
  duplicate (Env e a) = Env e (Env e a)
  extend f env@(Env e a) = Env e (f env)

ask :: Env e a -> e
ask (Env e _) = e

asks :: (e -> e') -> Env e a -> e'
asks f (Env e _) = f e

data Settings
  = Settings
      { padAmount :: Int,
        maxLength :: Int,
        padChar :: Char
      }
  deriving (Show)

getPadChar :: Env Settings a -> Char
getPadChar w = asks padChar w

env = Env

context :: Env Settings String
context =
  env
    (Settings {padAmount = 3, maxLength = 5, padChar = '*'})
    "Hello world"

-- >>> getPadChar context
-- '*'
-- >>> asks padAmount context
-- 3

trunc :: Env Settings [a] -> [a]
trunc w =
  let mxLngth = asks maxLength w
   in take mxLngth (extract w)

-- >>> trunc context
-- "Hello"

pad :: Env Settings String -> String
pad w =
  let padAmt = asks padAmount w
      c = asks padChar w
   in replicate padAmt c
        <> extract w
        <> replicate padAmt c

-- >>> pad context
-- "***Hello world***"

(=>=) ::
  Comonad w =>
  (w a -> b) ->
  (w b -> c) ->
  (w a -> c)
f =>= g = g . extend f

-- >>> (trunc =>= pad) context
-- "***Hello***"

-- >>> (pad =>= trunc) context
-- "***He"

local :: (e -> e') -> Env e a -> Env e' a
local f (Env e a) = Env (f e) a

-- |
-- >>> ((pad . local (\e -> e { padChar = '_' })) =>= trunc) context
-- "___He"

-- |
-- >>> (pad =>= (trunc  . local (\e -> e { maxLength = 9 }))) context
-- "***Hello "
pad' :: Env Settings String -> String
pad' = do
  padAmt <- asks padAmount
  c <- asks padChar
  txt <- extract
  let padding = replicate padAmt c
  pure (padding <> txt <> padding)

-- pad :: Env Settings String -> String
-- pad w =
--   let padAmt = asks padAmount w
--       c = asks padChar w
--    in replicate padAmt c
--         <> extract w
--         <> replicate padAmt c

-- | AKA Co-Writer
newtype Traced m a = Traced (m -> a)
  deriving (Functor)

instance Monoid m => Comonad (Traced m) where
  extract (Traced ma) = ma mempty
  duplicate (Traced ma) = Traced \m -> Traced \m' -> ma (m <> m')

-- extend g traced@(Traced ma) = Traced \m -> (\_ -> g traced) (ma m)

trace :: m -> Traced m a -> a
trace m (Traced ma) = ma m

adder :: Traced [Int] Int
adder = Traced sum

traced = Traced

-- >>>
-- >>> let adder' = extend (trace [1,2,3]) adder
-- >>> extract adder'
-- 6
-- >>> trace [10] adder'
-- 16

newBuilder :: Traced (Dual [String]) String
newBuilder = traced (concat . getDual)

logMsg :: Traced (Dual [String]) String -> String
logMsg = trace (Dual ["hello "]) =>= trace (Dual ["world"])
-- >>> logMsg newBuilder
-- "hello world"
