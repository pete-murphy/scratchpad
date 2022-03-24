module Trend where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Contravariant (Contravariant (..), Predicate)
import           Data.Monoid

data Trend
  = Up
  | Down
  | Flat
  deriving (Enum)

getTrendFromScoreDifference :: (Ord a, Num a) => a -> Trend
getTrendFromScoreDifference scoreDifference
  | scoreDifference > 0 = Up
  | scoreDifference < 0 = Down
  | otherwise = Flat

-- getTFSD' :: (Ord a, Num a) => a -> Trend
getTFSD :: (Ord a, Num a) => a -> Maybe a
getTFSD sd = do
  sd' <-
    if sd > 0
      then pure sd
      else fail mempty
  sd'' <-
    if sd < 0
      then pure sd
      else fail mempty
  sd''' <-
    if sd == 0
      then pure sd
      else fail mempty
  getFirst $ foldMap First $ Just <$> [sd', sd'', sd''']
