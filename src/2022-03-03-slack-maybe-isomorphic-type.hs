{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import GHC.Generics
import Generic.Data

data TreeNode a = Child a | Root
  deriving stock (Generic, Generic1)
  deriving (Functor, Applicative) via Generically1 Maybe
