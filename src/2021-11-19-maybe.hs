{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Functor.Identity
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal)
import Prelude hiding (Maybe (..))

data Maybe a = Nothing | Just a
  deriving (Show, Eq, Functor)

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  Just f <*> x = fmap f x
-- scanMap :: (Monoid m, Traversable t) => (a -> m) -> t a -> t m
-- scanMap f = fst . traverse ((mempty,) . flip . f)
