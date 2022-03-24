{-# LANGUAGE DeriveFunctor, TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

import qualified Test.QuickCheck.Classes as QuickCheck.Classes
import qualified Test.QuickCheck as QuickCheck
import Data.Proxy (Proxy(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Control.Applicative (Applicative(..))

data ValidationThese err a
  = Failure err
  | Both err a
  | Success a
  deriving (Eq, Functor, Show)

instance Semigroup err => Applicative (ValidationThese err) where
  pure = Success
  liftA2 _ (Failure err) (Failure err') = Failure (err <> err')
  liftA2 _ (Failure err) (Both err' _)  = Failure (err <> err')
  liftA2 _ (Failure err) (Success _)    = Failure err
  liftA2 _ (Both err a)  (Failure err') = Failure (err <> err')
  liftA2 f (Both err a)  (Both err' b)  = Both (err <> err') (f a b)
  liftA2 f (Both err a)  (Success b)    = Both err (f a b)
  liftA2 _ (Success a)   (Failure err)  = Failure err
  liftA2 f (Success a)   (Both err b)   = Both err (f a b)
  liftA2 f (Success a)   (Success b)    = Success (f a b)
  -- Failure err <*> Failure err' = Failure (err <> err')
  -- Failure err <*> Both err' _  = Failure (err <> err')
  -- Failure err <*> Success _    = Failure err
  -- Both err _  <*> Failure err' = Failure (err <> err')
  -- Both err f  <*> Both err' a  = Both (err <> err') (f a)
  -- Both err f  <*> Success a    = Both err (f a)
  -- Success _   <*> Failure err  = Failure err
  -- Success f   <*> Both err a   = Both err (f a)
  -- Success f   <*> Success a    = Success (f a)

-- instance (Semigroup a) => Applicative (These a) where
--     pure =Success 
--     Failure a <*> _         = Failure a
--     Success _ <*> Failure  b   = Failure b
--     Success f <*> Success    x = Success (f x)
--     Success f <*> Both b x = Both b (f x)
--     Both a _ <*> Failure  b   = Failure (a <> b)
--     Both a f <*> Success    x = Both a (f x)
--     Both a f <*> Both b x = Both (a <> b) (f x)



instance (Arbitrary err, Arbitrary a) => Arbitrary (ValidationThese err a) where 
  arbitrary = QuickCheck.oneof [
      Failure <$> arbitrary,
      Both    <$> arbitrary <*> arbitrary,
      Success <$> arbitrary
    ]

test :: IO ()
test = QuickCheck.Classes.lawsCheck do
  QuickCheck.Classes.applicativeLaws (Proxy @(ValidationThese [Int]))