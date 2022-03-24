{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

-- 00100
-- 11110
-- 10110
-- 10111
-- 10101
-- 01111
-- 00111
-- 11100
-- 10000
-- 11001
-- 00010
-- 01010

import Control.Applicative
import Control.Arrow
-- import Control.Lens (un, view)

import Control.Monad.State
import Data.Char
import Data.Coerce (coerce)
import Data.Function
import Data.Functor.Compose
import Data.Monoid (Ap (..), Sum (..))

newtype Line = Line [Int]
  deriving (Semigroup, Monoid) via (Ap ZipList (Sum Int))
  deriving stock (Show)

parseLine :: String -> Line
parseLine = map digitToInt >>> Line

fromBinary :: [Int] -> Int
fromBinary = reverse >>> go 1
  where
    go _ [] = 0
    go n (x : xs) = x * n + go (n * 2) xs

main = do
  lines' <- lines <$> readFile "src/aoc-2021/3-input"
  let lines'' =
        lines'
          & foldMap parseLine
          & coerce
      gamma =
        lines''
          & map (\x -> if x > length lines' `div` 2 then 1 else 0)
          & fromBinary
      epsilon =
        lines''
          & map (\x -> if x > length lines' `div` 2 then 0 else 1)
          & fromBinary
  print (gamma * epsilon)

-- |
-- >>> main
thing :: StateT String IO String
thing = do
  filename <- get
  lift (readFile filename)
