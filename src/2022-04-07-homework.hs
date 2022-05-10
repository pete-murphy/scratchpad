{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative (Applicative (..))
import Data.Proxy (Proxy (..))
import GHC.Generics
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck (Gen, vector)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

-- Purpose: given a Thingy, produce the sum of yaks in a thingy
data Thingy
  = Yak Int
  | Breeze Color Bool
  | Cabinet Thingy (List Bool)
  | Can (List (Maybe Thingy))
  deriving (Show)

data Color = Red | Blue | Green deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

fromList' :: List a -> [a]
fromList' Nil = []
fromList' (Cons x xs) = x : fromList' xs

toList' :: [a] -> List a
toList' = foldr Cons Nil

instance Arbitrary Color where
  arbitrary = QuickCheck.elements [Red, Blue, Green]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    QuickCheck.frequency
      [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]

instance Arbitrary Thingy where
  arbitrary =
    QuickCheck.oneof
      [ Yak <$> arbitrary,
        Breeze <$> arbitrary <*> arbitrary,
        Cabinet <$> arbitrary <*> (toList' <$> vector 10),
        Can <$> (arbitrary :: Gen (List (Maybe Thingy)))
      ]

-- data Color = Red | Green | Blue deriving (Show)

-- data List a = Nil | Cons a (List a) deriving (Show)

data Stuff a
  = Buffalo a
  | Wind Color Bool
  | Drawer (Stuff a) (List Bool)
  | Cant (List (Maybe (Stuff a)))
  deriving (Show)

foldrList :: (b -> a -> b) -> b -> List a -> b
foldrList f b (Cons x xs) = foldrList f (f b x) xs
foldrList f b Nil = b

-- >>> foldrList (+) 0 (Cons 1 (Cons 2 (Cons 3 Nil)))
-- 6

foldrList (+) 0 (Cons 1 (Cons 2 (Cons 3 Nil)))
foldrList (+) (0 + 1) (Cons 2 (Cons 3 Nil))
foldrList (+) ((0 + 1) + 2) (Cons 3 Nil)
foldrList (+) (((0 + 1) + 2) + 3) Nil
              (((0 + 1) + 2) + 3)

-- >>> foldr (-) 0 [1,2,3]
-- 2

-- >>> 

-- foldrList' :: (a -> b -> b) -> b -> List a -> b
-- foldrList' f b (Cons x xs) = f x ()
-- foldrList' f b Nil = b


-- Implement this
extraCredit1 :: Num a => Stuff a -> a
extraCredit1 = extraCredit2 id

-- What needs to change if we don't know the values in Stuff?
-- HINT: This compiles, but you won't be able to implement it as is. You'll need more inputs to make this work
extraCredit2 :: Num b => (a -> b) -> Stuff a -> b
extraCredit2 f stuff = go stuff 0
  where
    go (Buffalo a) acc = acc + f a
    go (Drawer s _) acc = go s acc
    go (Cant mss) acc = foldrList (\b -> maybe b (`go` b)) acc mss
    go _ acc = acc

stuff1 :: Stuff Int
stuff1 = Buffalo 20

-- 20

stuff2 :: Stuff Int
stuff2 = Cant (Cons (Just (Buffalo 5)) (Cons (Just (Buffalo 2)) (Cons (Just (Wind Red True)) (Cons (Just (Drawer stuff1 Nil)) Nil))))

-- 27

stuff3 :: Stuff Int
stuff3 = Cant (Cons (Just stuff3) Nil)
-- Never ends
