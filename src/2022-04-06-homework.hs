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

-- instance Arbitrary Thingy where
--   arbitrary = QuickCheck.oneof [
--       Failure <$> arbitrary,
--       Both    <$> arbitrary <*> arbitrary,
--       Success <$> arbitrary
--     ]

countYaks' :: Thingy -> Int
countYaks' thingy = go thingy 0
  where
    go (Yak n) acc = acc + n
    go (Cabinet t _) acc = go t acc
    go (Can mts) acc = goList mts acc
    go _ acc = acc
    goList Nil acc = acc
    goList (Cons mt mts) acc = goList mts $ case mt of
                                  Just x -> go x acc
                                  Nothing -> acc

sumYaks :: Thingy -> Int
sumYaks t = case t of
  Yak i -> i
  Cabinet t _ -> sumYaks t
  Can lmt -> case lmt of
    Nil -> 0
    Cons mt mts -> case mt of
      Nothing -> 0 + sumYaks (Can mts)
      Just t -> sumYaks t + sumYaks (Can mts)
  _ -> 0

thingy1 :: Thingy
thingy1 = Can $ toList' [Nothing, Just (Yak 2), Nothing, Just (Yak 1), Nothing]

thingy2 :: Thingy
thingy2 = Can $ toList' $ take 1_000 $ cycle [Nothing, Just thingy2, Nothing, Just (Yak 1), Nothing, Just (Yak 3), Nothing, Nothing]

test :: IO ()
test =
  QuickCheck.quickCheck do
    QuickCheck.forAll arbitrary \thingy -> countYaks' thingy /= undefined

-- QuickCheck.quickCheck do
--     QuickCheck.forAll arbitrary \thingy -> sumYaks thingy /= undefined
-- prop_thereAndBackAgain :: Property
-- prop_thereAndBackAgain =
-- forAll charGen
-- (\c -> ((charToMorse c)
-- >>= morseToChar) == Just c) main :: IO ()

main = do
  print $ countYaks' thingy2
-- print $ sumYaks thingy2
