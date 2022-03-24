{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens hiding (partsOf)
import Data.Monoid (Endo (..))

partsOf :: Traversal' s a -> Lens' s [a]
partsOf t = lens getter setter
  where
    getter s = s ^.. t
    setter s as = appEndo (foldMap (\a -> Endo (t .~ a)) as) s
