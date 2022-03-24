{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}

module EitherAp where

import Data.Monoid (Ap (..))
import Prelude hiding (Either (..))
import qualified Data.Either as E

-- data Either a b = Left a | Right b
--   deriving stock (Eq, Ord, Show)
--   deriving (Functor, Applicative, Monad)
--     via (E.Either a)
--   deriving (Semigroup, Monoid)
--     via (Ap (E.Either a) b)

type MyEither a b = Ap (E.Either a) b