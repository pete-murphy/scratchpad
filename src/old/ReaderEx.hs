module ReaderEx where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <*> rev <$> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  pure (a, b)

combineReaders :: (r -> a) -> (a -> r -> b) -> r -> b
--combineReaders :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
combineReaders ra arb = \r ->
  let a = ra r; b = arb a r in b