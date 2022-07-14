{-# LANGUAGE TemplateHaskell #-}

import Data.Functor.Classes (Show1 (..), showsPrec1)
import Text.Show.Deriving (deriveShow1)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

$(deriveShow1 ''EitherT)

instance (Show1 m, Show e, Show a) => Show (EitherT e m a) where
  showsPrec = showsPrec1

-- >>> show (EitherT (Just (Left 1)))
-- "EitherT {runEitherT = Just (Left 1)}"
