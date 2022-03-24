{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader, ReaderT)
import Data.Bifunctor (Bifunctor)
import Data.Bifunctor.Tannen (Tannen)

newtype ReaderTaskEither r e a
  = ReaderTaskEither {runReaderTaskEither :: ReaderT r (ExceptT e IO) a}

-- Tannen f p a b = f (p a b)
--

deriving instance Functor (ReaderTaskEither r e)
-- newtype X r e a = X (Tannen (ReaderT r) ExceptT e a)

-- deriving via Bifunctor  instance Bifunctor (ReaderTaskEither r)
