{-# LANGUAGE QuantifiedConstraints #-}

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (forall x. Show x => Show (m x), Show e, Show a) => Show (EitherT e m a) where
  show (EitherT x) = "EitherT {runEitherT = " <> show x <> "}"
-- >>> show (EitherT (Just (Left 1)))
-- "EitherT {runEitherT = Just (Left 1)}"
