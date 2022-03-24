module QuickCheckSample where

import Test.QuickCheck
import Control.Monad.Identity

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

-- instance Arbitrary a => Arbitrary (Identity a) where
--   arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen
