{-# LANGUAGE RankNTypes #-}

import Control.Lens

underMaybe :: Lens' s a -> Lens' (Maybe s) (Maybe a)
underMaybe l = _ . l . _sdf
