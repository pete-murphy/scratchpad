{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow

-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE PolyKinds #-}

-- import Data.Functor.Classes (Show1)
-- import Data.Functor.Classes
-- import Data.Functor.Classes.Generic
-- import GHC.Generics

-- data CodebaseServerOpts f
--   = CodebaseServerOpts
--       { token :: f String,
--         host :: f String,
--         port :: f Int,
--         codebaseUIPath :: f FilePath
--       }
--   deriving
--     (Eq1, Ord1, Read1, Show1)
--     via FunctorClassesDefault (CodebaseServerOpts)

-- instance (Show1 f) => Show (CodebaseServerOpts f) where
--   show = _what

newtype WriterlikeT w m a = WriterlikeT (w, m a)
  deriving (Functor)

instance (Monoid w, Applicative m) => Applicative (WriterlikeT w m) where
  pure a = WriterlikeT (mempty, pure a)
  WriterlikeT (w, ma) <*> WriterlikeT (w', mf) = WriterlikeT (w <> w', ma <*> mf)

instance (Monoid w, Monad m) => Monad (WriterlikeT w m) where
  (>>=) :: WriterlikeT w m a -> (a -> WriterlikeT w m b) -> WriterlikeT w m b
  -- (w, m b)
  WriterlikeT (w, ma) >>= f = WriterlikeT $ (w,) do
    a <- ma
    let WriterlikeT (_, mb) = f a
    mb
