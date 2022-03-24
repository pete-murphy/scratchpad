{-# LANGUAGE DataKinds #-}

import Data.Proxy (Proxy)
import GHC.Base (Type)
import GHC.TypeLits (KnownNat)

foo :: (KnownNat n) => Proxy n -> [Type] -> Type
foo = undefined
