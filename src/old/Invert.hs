{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

module Invert where

import qualified Data.Map.Strict        as Map
import           Generics.Deriving.Enum (GEnum (genum))
import           GHC.Generics           (Generic)

data Product
  = Basic
  | Standard
  | Pro
  deriving (Generic, Show)
  deriving anyclass (GEnum)

data Frequency
  = Monthly
  | Annual
  deriving (Generic, Show)
  deriving anyclass (GEnum)

data Bill =
  Bill Product Frequency
  deriving (Generic, Show)
  deriving anyclass (GEnum)

encodeProduct :: Product -> String
encodeProduct =
  \case
    Basic -> "p1"
    Standard -> "p2"
    Pro -> "p3"

encodeBill :: Bill -> Integer
encodeBill =
  \case
    Bill Basic Monthly -> 10
    Bill Basic Annual -> 11
    Bill Standard Monthly -> 20
    Bill Standard Annual -> 21
    Bill Pro Monthly -> 30
    Bill Pro Annual -> 31

invert :: (GEnum a, Ord b) => (a -> b) -> b -> Maybe a
invert f =
  let reverseMap = foldMap (\a -> Map.singleton (f a) a) genum
   in (`Map.lookup` reverseMap)

decodeProduct :: String -> Maybe Product
decodeProduct = invert encodeProduct

decodeBill :: Integer -> Maybe Bill
decodeBill = invert encodeBill

main = do
  putStrLn (encodeProduct Basic)
  putStrLn (encodeProduct Standard)
  putStrLn (show $ decodeProduct "p1")
  putStrLn (show $ decodeProduct "xyz")
  putStrLn (show $ encodeBill $ Bill Basic Annual)
  putStrLn (show $ encodeBill $ Bill Pro Monthly)
  putStrLn (show $ decodeBill 31)
  putStrLn (show $ decodeBill 50)
